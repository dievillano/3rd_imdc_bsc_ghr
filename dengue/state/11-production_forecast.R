# =============================================================================
# 11-production_forecast.R
#
# Purpose:
#   Fit the legacy BSC-GHR dengue model for the final IMDC 2026 production
#   forecast using:
#
#     - historical dengue data through 2025 from dengue.csv.gz
#     - the organiser's updated 2026 surveillance snapshot through EW25
#     - the operational climate pathway in
#       climate/fcst_target/out_files/clim_forecast_2026.csv
#     - the same scaling, interaction construction, INLA model, posterior
#       predictive sampling and state aggregation used in the validation round
#
# Forecast target:
#   EW41 2026 through EW40 2027
#
# Important:
#   - The original validation dengue file is NOT modified.
#   - Updated 2026 surveillance values replace the old 2026 values completely.
#   - The organiser train_*/target_* flags are NOT used for the production split.
#   - Climate predictors are scaled using production TRAINING rows only.
#   - Interaction terms are created AFTER scaling, via add_model_vars().
# =============================================================================

inla_lib <- Sys.getenv("EBROOTRMININLA")

if (
  nzchar(inla_lib) &&
  dir.exists(file.path(inla_lib, "INLA"))
) {
  .libPaths(c(inla_lib, .libPaths()))
}

cat("INLA library:", find.package("INLA"), "\n")
cat("INLA version:", as.character(packageVersion("INLA")), "\n")
INLA::inla.version()

# =============================================================================
# 0. Paths and configuration
# =============================================================================

sprint2026_path <- here::here()

data_path <- fs::path(
  sprint2026_path,
  "data"
)

raw_data_path <- fs::path(
  data_path,
  "raw/data_imdc_2026"
)

interim_data_path <- fs::path(
  data_path,
  "interim/health_region"
)

processed_data_path <- fs::path(
  data_path,
  "processed/health_region"
)

resources_path <- fs::path(
  sprint2026_path,
  "resources/health_region"
)

utils_path <- fs::path(
  sprint2026_path,
  "utils"
)

dengue_path <- fs::path(
  sprint2026_path,
  "dengue/state"
)

output_path <- fs::path(
  dengue_path,
  "outputs/production"
)

fs::dir_create(
  output_path,
  recurse = TRUE
)

utils_filepaths <- fs::dir_ls(
  utils_path,
  type = "file"
)

purrr::walk(
  utils_filepaths,
  source
)

# setup_hpc_library()


# Input files ------------------------------------------------------------------

dengue_old_file <- fs::path(
  raw_data_path,
  "dengue.csv.gz"
)

dengue_update_file <- fs::path(
  raw_data_path,
  "dengue_update_2026.csv.gz"
)

climate_file <- fs::path(
  sprint2026_path,
  "climate/fcst_target/out_files/clim_forecast_2026.csv"
)

population_file <- fs::path(
  interim_data_path,
  "population_hr.rds"
)

environ_file <- fs::path(
  interim_data_path,
  "environ_hr.rds"
)


# The data-preparation script writes the graph under resources/, while some
# validation scripts read a copy under data/processed/. Prefer the canonical
# resources path but accept the validation location if that is the one present.

graph_candidates <- c(
  fs::path(
    resources_path,
    "graph/hr_graph.graph"
  ),
  fs::path(
    processed_data_path,
    "graph/hr_graph.graph"
  )
)

graph_exists <- fs::file_exists(
  graph_candidates
)

if (!any(graph_exists)) {
  stop(
    "Health-region INLA graph not found. Checked:\n",
    paste(
      graph_candidates,
      collapse = "\n"
    )
  )
}

graph_file <- graph_candidates[
  which(graph_exists)[1L]
]


# Forecast configuration -------------------------------------------------------

drop_state <- 32L

last_observed_epiweek <- 202625L

target_start_epiweek <- 202641L
target_end_epiweek <- 202740L

n_expected_regions <- 435L
n_samples <- 1000L
seed <- 2026L

climate_predictors <- c(
  "tasan6.l1",
  "spei3.l1",
  "spei12.l3",
  "tas6.l1",
  "oni.l6"
)

wis_quantiles <- c(
  0.025,
  0.05,
  0.10,
  0.25,
  0.50,
  0.75,
  0.90,
  0.95,
  0.975
)


# Helper used only to construct/check future epidemiological-week codes.
# Before using it prospectively, the script verifies that it reproduces all
# observed organiser epiweek codes in the 2026 update.

make_epiweek_code <- function(date) {

  as.integer(
    sprintf(
      "%d%02d",
      lubridate::epiyear(date),
      lubridate::epiweek(date)
    )
  )
}


# =============================================================================
# 1. Read and validate the updated dengue surveillance snapshot
# =============================================================================

dengue_old <- readr::read_csv(
  dengue_old_file,
  show_col_types = FALSE
)

dengue_update <- readr::read_csv(
  dengue_update_file,
  show_col_types = FALSE
)

required_dengue_vars <- c(
  "geocode",
  "date",
  "casos",
  "epiweek",
  "uf",
  "regional_geocode",
  "uf_code",
  "disease"
)

missing_old_vars <- setdiff(
  required_dengue_vars,
  names(dengue_old)
)

missing_update_vars <- setdiff(
  required_dengue_vars,
  names(dengue_update)
)

if (length(missing_old_vars) > 0L) {
  stop(
    "Missing variables in old dengue data: ",
    paste(
      missing_old_vars,
      collapse = ", "
    )
  )
}

if (length(missing_update_vars) > 0L) {
  stop(
    "Missing variables in updated dengue data: ",
    paste(
      missing_update_vars,
      collapse = ", "
    )
  )
}


# Updated snapshot should contain one row per municipality-week.

update_duplicates <- dengue_update |>
  dplyr::count(
    geocode,
    epiweek,
    name = "n"
  ) |>
  dplyr::filter(
    n != 1L
  )

if (nrow(update_duplicates) > 0L) {
  stop(
    "Updated dengue data contain duplicated municipality-week rows."
  )
}

if (anyNA(dengue_update$casos)) {
  stop(
    "Updated dengue data contain missing case counts."
  )
}


# The update should end exactly at the production observation cutoff.

if (
  max(
    dengue_update$epiweek,
    na.rm = TRUE
  ) != last_observed_epiweek
) {
  stop(
    "Unexpected final epiweek in updated dengue data. Expected ",
    last_observed_epiweek,
    " but found ",
    max(
      dengue_update$epiweek,
      na.rm = TRUE
    ),
    "."
  )
}


# Verify that lubridate's epidemiological-week convention reproduces the
# organiser's observed epiweek codes before using it to construct future weeks.

update_epiweek_check <- make_epiweek_code(
  dengue_update$date
)

if (
  any(
    update_epiweek_check != dengue_update$epiweek
  )
) {
  stop(
    "lubridate epiweek/epiyear does not reproduce the organiser epiweek codes."
  )
}


# Check that the municipality universe is unchanged.

if (
  !setequal(
    unique(
      dengue_old$geocode
    ),
    unique(
      dengue_update$geocode
    )
  )
) {
  stop(
    "Municipality set differs between old and updated dengue data."
  )
}


# Audit overlapping revisions.

dengue_overlap <- dengue_old |>
  dplyr::inner_join(
    dengue_update,
    by = c(
      "geocode",
      "date",
      "epiweek"
    ),
    suffix = c(
      "_old",
      "_new"
    )
  )

if (nrow(dengue_overlap) > 0L) {

  geography_changes <- dengue_overlap |>
    dplyr::summarise(
      uf_code_diff = sum(
        uf_code_old != uf_code_new,
        na.rm = TRUE
      ),
      regional_geocode_diff = sum(
        regional_geocode_old != regional_geocode_new,
        na.rm = TRUE
      ),
      uf_diff = sum(
        uf_old != uf_new,
        na.rm = TRUE
      ),
      disease_diff = sum(
        disease_old != disease_new,
        na.rm = TRUE
      )
    )

  if (
    any(
      unlist(
        geography_changes
      ) != 0L
    )
  ) {
    stop(
      "Geographic/disease identifiers changed in the dengue overlap."
    )
  }

  n_case_revisions <- sum(
    dengue_overlap$casos_old != dengue_overlap$casos_new,
    na.rm = TRUE
  )

  cat(
    "\nDengue update audit:\n",
    "  overlapping municipality-weeks: ",
    nrow(dengue_overlap),
    "\n",
    "  revised case counts: ",
    n_case_revisions,
    "\n",
    sep = ""
  )
}


# =============================================================================
# 2. Assemble authoritative observed dengue history
# =============================================================================
#
# Keep the old organiser history before the start of the 2026 update.
# Replace the complete 2026 period with the updated organiser snapshot.
# =============================================================================

update_start_date <- min(
  dengue_update$date
)

dengue_obs_muni <- dengue_old |>
  dplyr::filter(
    date < update_start_date
  ) |>
  dplyr::bind_rows(
    dengue_update
  ) |>
  dplyr::arrange(
    geocode,
    date
  )


# Production modelling excludes state code 32 exactly as in the sprint pipeline.

dengue_obs_muni <- dengue_obs_muni |>
  dplyr::filter(
    uf_code != drop_state
  )


# Aggregate municipality cases to health-region level.

dengue_obs_hr <- dengue_obs_muni |>
  dplyr::group_by(
    uf_code,
    uf,
    regional_geocode,
    date,
    epiweek
  ) |>
  dplyr::summarise(
    cases = sum(
      casos
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    regional_geocode,
    date
  )


# Basic observed-panel checks.

if (
  dplyr::n_distinct(
    dengue_obs_hr$regional_geocode
  ) != n_expected_regions
) {
  stop(
    "Unexpected number of health regions in observed dengue data."
  )
}

if (
  max(
    dengue_obs_hr$epiweek,
    na.rm = TRUE
  ) != last_observed_epiweek
) {
  stop(
    "Observed health-region panel does not end at EW25 2026."
  )
}

if (anyNA(dengue_obs_hr$cases)) {
  stop(
    "Observed health-region panel contains missing cases."
  )
}


# =============================================================================
# 3. Extend weekly health-region panel through EW40 2027
# =============================================================================
#
# Generate future Sundays from the last observed week. Epiweek codes are
# calculated from dates only after validating the convention against the
# organiser's observed 2026 data above.
# =============================================================================

future_dates_all <- seq.Date(
  from = max(
    dengue_obs_hr$date
  ) + 7,
  to = as.Date(
    "2027-12-31"
  ),
  by = "week"
)

future_week_table <- tibble::tibble(
  date = future_dates_all,
  epiweek = make_epiweek_code(
    future_dates_all
  )
) |>
  dplyr::filter(
    epiweek <= target_end_epiweek
  )

if (
  max(
    future_week_table$epiweek
  ) != target_end_epiweek
) {
  stop(
    "Future weekly sequence does not reach EW40 2027."
  )
}

if (
  anyDuplicated(
    future_week_table$epiweek
  ) > 0L
) {
  stop(
    "Duplicated future epiweek codes were generated."
  )
}


hr_ids <- dengue_obs_hr |>
  dplyr::distinct(
    uf_code,
    uf,
    regional_geocode
  )

future_rows <- tidyr::crossing(
  hr_ids,
  future_week_table
) |>
  dplyr::mutate(
    cases = NA_real_
  ) |>
  dplyr::select(
    dplyr::all_of(
      names(
        dengue_obs_hr
      )
    )
  )


dengue_full <- dplyr::bind_rows(
  dengue_obs_hr,
  future_rows
) |>
  dplyr::mutate(
    month = lubridate::month(
      date + 3
    ),
    year = lubridate::year(
      date + 3
    )
  ) |>
  dplyr::arrange(
    regional_geocode,
    date
  )


# =============================================================================
# 4. Prepare operational monthly climate predictors and legacy lags
# =============================================================================

climate_raw <- readr::read_csv(
  climate_file,
  show_col_types = FALSE
)

required_climate_vars <- c(
  "regional_geocode",
  "month",
  "year",
  "date",
  "tas6",
  "oni",
  "tasan6",
  "spei3",
  "spei12"
)

missing_climate_vars <- setdiff(
  required_climate_vars,
  names(
    climate_raw
  )
)

if (
  length(
    missing_climate_vars
  ) > 0L
) {
  stop(
    "Missing variables in operational climate file: ",
    paste(
      missing_climate_vars,
      collapse = ", "
    )
  )
}

climate_raw <- climate_raw |>
  dplyr::mutate(
    regional_geocode = as.integer(
      regional_geocode
    ),
    date = as.Date(
      date
    )
  ) |>
  dplyr::arrange(
    regional_geocode,
    date
  )


# Climate file should have exactly one row per HR-month.

climate_duplicates <- climate_raw |>
  dplyr::count(
    regional_geocode,
    year,
    month,
    name = "n"
  ) |>
  dplyr::filter(
    n != 1L
  )

if (
  nrow(
    climate_duplicates
  ) > 0L
) {
  stop(
    "Operational climate file contains duplicated HR-month rows."
  )
}


# Ensure the operational climate geography matches the dengue model geography.

if (
  !setequal(
    unique(
      climate_raw$regional_geocode
    ),
    unique(
      dengue_obs_hr$regional_geocode
    )
  )
) {
  stop(
    "Health-region IDs differ between dengue and climate data."
  )
}


# Create only the five lags required by the legacy model.

climate_lag <- climate_raw |>
  dplyr::group_by(
    regional_geocode
  ) |>
  dplyr::arrange(
    date,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    tasan6.l1 = dplyr::lag(
      tasan6,
      1L
    ),
    spei3.l1 = dplyr::lag(
      spei3,
      1L
    ),
    spei12.l3 = dplyr::lag(
      spei12,
      3L
    ),
    tas6.l1 = dplyr::lag(
      tas6,
      1L
    ),
    oni.l6 = dplyr::lag(
      oni,
      6L
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    regional_geocode,
    month,
    year,
    dplyr::all_of(
      climate_predictors
    )
  )


# =============================================================================
# 5. Read environmental data and extend population through 2027
# =============================================================================

environ <- readRDS(
  environ_file
)

population <- readRDS(
  population_file
)


# Current sprint preparation creates population through 2026. For the 2027
# forecast year, carry the same endpoint estimate forward one additional year.

if (
  !2027L %in% population$year
) {

  population_2027 <- population |>
    dplyr::filter(
      year == 2026L
    )

  if (
    nrow(
      population_2027
    ) == 0L
  ) {
    stop(
      "No 2026 population rows available to extend to 2027."
    )
  }

  population_2027 <- population_2027 |>
    dplyr::mutate(
      year = 2027L
    )

  population <- dplyr::bind_rows(
    population,
    population_2027
  )
}


# =============================================================================
# 6. Merge the complete production modelling dataset
# =============================================================================

data <- dengue_full |>
  dplyr::left_join(
    climate_lag,
    by = c(
      "regional_geocode",
      "month",
      "year"
    )
  ) |>
  dplyr::left_join(
    environ,
    by = c(
      "uf_code",
      "uf",
      "regional_geocode"
    )
  ) |>
  dplyr::left_join(
    population,
    by = c(
      "uf_code",
      "uf",
      "regional_geocode",
      "year"
    )
  ) |>
  dplyr::arrange(
    regional_geocode,
    date
  ) |>
  dplyr::mutate(
    hr_id = as.integer(
      factor(
        regional_geocode
      )
    ),
    state_id = as.integer(
      factor(
        uf_code
      )
    ),
    time_id = as.integer(
      factor(
        epiweek
      )
    ),
    epiweek_num = as.integer(
      substr(
        epiweek,
        5,
        6
      )
    ),
    epiweek_num = dplyr::if_else(
      epiweek_num == 53L,
      52L,
      epiweek_num
    ),
    week_id = dplyr::if_else(
      epiweek_num >= 41L,
      epiweek_num - 40L,
      epiweek_num + 12L
    ),
    month_id = month,
    epiyear = dplyr::if_else(
      epiweek_num <= 40L,
      year,
      year + 1L
    ),
    year_id = as.integer(
      factor(
        epiyear
      )
    ),
    koppen = factor(
      koppen
    ),
    koppen_id = as.integer(
      koppen
    ),
    biome = factor(
      biome
    ),
    biome_id = as.integer(
      biome
    ),
    pop100k = population / 100000
  )


# =============================================================================
# 7. Define production training and forecast sets
# =============================================================================

train <- data |>
  dplyr::filter(
    epiweek <= last_observed_epiweek
  )

forecast <- data |>
  dplyr::filter(
    dplyr::between(
      epiweek,
      target_start_epiweek,
      target_end_epiweek
    )
  )


# Production split checks -------------------------------------------------------

if (
  max(
    train$epiweek
  ) != last_observed_epiweek
) {
  stop(
    "Production training data do not end at EW25 2026."
  )
}

if (
  anyNA(
    train$cases
  )
) {
  stop(
    "Production training data contain missing case counts."
  )
}

if (
  !all(
    is.na(
      forecast$cases
    )
  )
) {
  stop(
    "Production forecast rows should have cases = NA."
  )
}

if (
  dplyr::n_distinct(
    forecast$date
  ) != 52L
) {
  stop(
    "Production forecast target must contain exactly 52 weeks."
  )
}

if (
  min(
    forecast$epiweek
  ) != target_start_epiweek ||
    max(
      forecast$epiweek
    ) != target_end_epiweek
) {
  stop(
    "Unexpected production forecast epiweek range."
  )
}

if (
  dplyr::n_distinct(
    forecast$regional_geocode
  ) != n_expected_regions
) {
  stop(
    "Unexpected number of health regions in production forecast data."
  )
}


# Required modelling covariates must be complete in both train and forecast.

required_model_vars <- c(
  climate_predictors,
  "population",
  "pop100k",
  "koppen",
  "koppen_id",
  "hr_id",
  "state_id",
  "week_id",
  "epiyear",
  "year_id"
)

missing_train_covariates <- train |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        required_model_vars
      ),
      ~ sum(
        is.na(
          .x
        )
      )
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) |>
  dplyr::filter(
    n_missing > 0L
  )

missing_forecast_covariates <- forecast |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        required_model_vars
      ),
      ~ sum(
        is.na(
          .x
        )
      )
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) |>
  dplyr::filter(
    n_missing > 0L
  )

if (
  nrow(
    missing_train_covariates
  ) > 0L
) {
  print(
    missing_train_covariates
  )

  stop(
    "Missing modelling covariates in production training data."
  )
}

if (
  nrow(
    missing_forecast_covariates
  ) > 0L
) {
  print(
    missing_forecast_covariates
  )

  stop(
    "Missing modelling covariates in production forecast data."
  )
}

if (
  any(
    train$pop100k <= 0
  ) ||
    any(
      forecast$pop100k <= 0
    )
) {
  stop(
    "Population exposure must be strictly positive."
  )
}


# Metadata uses the same names expected by fit_forecast_inla().

production_metadata <- tibble::tibble(
  split_id = "production_2026",
  train_start_date = min(
    train$date
  ),
  train_end_date = max(
    train$date
  ),
  train_start_epiweek = min(
    train$epiweek
  ),
  train_end_epiweek = max(
    train$epiweek
  ),
  train_n_weeks = dplyr::n_distinct(
    train$date
  ),
  val_start_date = min(
    forecast$date
  ),
  val_end_date = max(
    forecast$date
  ),
  val_start_epiweek = min(
    forecast$epiweek
  ),
  val_end_epiweek = max(
    forecast$epiweek
  ),
  val_n_weeks = dplyr::n_distinct(
    forecast$date
  )
)


cat(
  "\nProduction split:\n"
)

print(
  production_metadata,
  width = Inf
)


# Save the unscaled assembled production dataset for reproducibility/QA.

saveRDS(
  data,
  fs::path(
    output_path,
    "production_dataset_unscaled.rds"
  )
)


# =============================================================================
# 8. Scale the five legacy climate predictors using TRAINING rows only
# =============================================================================

scaled <- scale_split(
  train_data = train,
  validation_data = forecast,
  predictors = tidyselect::all_of(
    climate_predictors
  ),
  overwrite = TRUE
)

saveRDS(
  scaled$scaler,
  fs::path(
    output_path,
    "production_scaler.rds"
  )
)


# Construct the legacy interaction variables AFTER scaling.

train_model <- add_model_vars(
  scaled$train
)

forecast_model <- add_model_vars(
  scaled$validation
)


# =============================================================================
# 9. Legacy model specification
# =============================================================================

hr_graph <- INLA::inla.read.graph(
  graph_file
)

prec_prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(
      0.5,
      0.01
    )
  )
)

formula_env <- rlang::env(
  hr_graph = hr_graph,
  prec_prior = prec_prior
)

re_s <- paste(
  "f(hr_id, model = 'bym2', graph = hr_graph, scale.model = TRUE,",
  "hyper = prec_prior, constr = TRUE)"
)

re_w <- paste(
  "f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
  "constr = TRUE, scale.model = TRUE, hyper = prec_prior)"
)

re_y <- paste(
  "f(year_id, model = 'iid', hyper = prec_prior)"
)

formula_string <- paste(
  "cases ~ 1",
  "v1 + v2 + v3 + v1v2 + v1v3 + v2v3 + v1v2v3",
  "tas6.l1 + oni.l6 + period_id",
  re_s,
  re_w,
  re_y,
  sep = " + "
)

cat(
  "\nLegacy production formula:\n",
  formula_string,
  "\n",
  sep = ""
)

cat(
  "\nUsing graph:\n",
  graph_file,
  "\n",
  sep = ""
)


# =============================================================================
# 10. Fit production model and obtain HR posterior predictive samples
# =============================================================================

out <- fit_forecast_inla(
  formula_string = formula_string,
  train_data = train_model,
  forecast_data = forecast_model,
  outcome = "cases",
  family = "nbinomial",
  exposure = "pop100k",
  offset = NULL,
  nthreads_inla = as.integer(
    Sys.getenv(
      "SLURM_CPUS_PER_TASK",
      "1"
    )
  ),
  forecast_ppd = TRUE,
  control_compute = list(
    config = TRUE
  ),
  control_predictor = list(
    compute = TRUE,
    link = 1
  ),
  n_samples = n_samples,
  formula_env = formula_env,
  split_metadata = production_metadata,
  keep_fit = FALSE,
  seed = seed,
  verbose = TRUE
)

hr_predictions <- out$predictions |>
  dplyr::mutate(
    .model_id = "legacy_model",
    .model_label = "Legacy model"
  ) |>
  dplyr::relocate(
    .model_id,
    .model_label
  )


if (
  nrow(
    hr_predictions
  ) != n_expected_regions * 52L
) {
  stop(
    "Unexpected number of HR-week prediction rows."
  )
}

if (
  !all(
    lengths(
      hr_predictions$.pred_samples
    ) == n_samples
  )
) {
  stop(
    "Unexpected number of posterior predictive samples in HR predictions."
  )
}

saveRDS(
  hr_predictions,
  fs::path(
    output_path,
    "legacy_model_hr_predictions.rds"
  )
)


# =============================================================================
# 11. Aggregate posterior predictive samples HR -> state, draw by draw
# =============================================================================

state_predictions <- hr_predictions |>
  dplyr::group_by(
    uf_code,
    uf,
    epiweek,
    date,
    .lead_week
  ) |>
  dplyr::summarise(
    .pred_samples = list(
      purrr::reduce(
        .pred_samples,
        `+`
      )
    ),
    .groups = "drop"
  )

if (
  !all(
    lengths(
      state_predictions$.pred_samples
    ) == n_samples
  )
) {
  stop(
    "Unexpected number of posterior predictive samples after state aggregation."
  )
}

saveRDS(
  state_predictions,
  fs::path(
    output_path,
    "legacy_model_state_predictions.rds"
  )
)


# =============================================================================
# 12. Create final state-level challenge quantiles
# =============================================================================

submission <- state_predictions |>
  dplyr::mutate(
    q = purrr::map(
      .pred_samples,
      \(x) stats::quantile(
        x,
        probs = wis_quantiles,
        na.rm = TRUE,
        names = FALSE
      )
    )
  ) |>
  tidyr::unnest_wider(
    q,
    names_sep = "_"
  ) |>
  dplyr::rename(
    lower_95 = q_1,
    lower_90 = q_2,
    lower_80 = q_3,
    lower_50 = q_4,
    pred = q_5,
    upper_50 = q_6,
    upper_80 = q_7,
    upper_90 = q_8,
    upper_95 = q_9
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        lower_95,
        lower_90,
        lower_80,
        lower_50,
        pred,
        upper_50,
        upper_80,
        upper_90,
        upper_95
      ),
      \(x) pmax(
        x,
        0
      )
    )
  ) |>
  dplyr::select(
    uf_code,
    uf,
    epiweek,
    date,
    pred,
    lower_50,
    upper_50,
    lower_80,
    upper_80,
    lower_90,
    upper_90,
    lower_95,
    upper_95
  ) |>
  dplyr::arrange(
    uf_code,
    date
  )


# =============================================================================
# 13. Final QA
# =============================================================================

if (
  !all(
    lubridate::wday(
      submission$date,
      week_start = 7
    ) == 1
  )
) {
  stop(
    "Submission contains dates that are not Sundays."
  )
}

if (
  !all(
    submission$lower_95 <= submission$lower_90,
    submission$lower_90 <= submission$lower_80,
    submission$lower_80 <= submission$lower_50,
    submission$lower_50 <= submission$pred,
    submission$pred <= submission$upper_50,
    submission$upper_50 <= submission$upper_80,
    submission$upper_80 <= submission$upper_90,
    submission$upper_90 <= submission$upper_95
  )
) {
  stop(
    "Submission quantiles are not monotonically ordered."
  )
}

if (
  any(
    submission$lower_95 < 0
  )
) {
  stop(
    "Submission contains negative prediction quantiles."
  )
}

submission_panel_check <- submission |>
  dplyr::count(
    uf_code,
    name = "n_weeks"
  )

if (
  any(
    submission_panel_check$n_weeks != 52L
  )
) {
  print(
    submission_panel_check
  )

  stop(
    "Not every state has exactly 52 forecast weeks."
  )
}


# Save a QA version retaining epiweek.

readr::write_csv(
  submission,
  fs::path(
    output_path,
    "state_forecast_submission_with_epiweek.csv"
  )
)


# Save the upload-ready version, matching the validation submission layout.

submission |>
  dplyr::select(
    -epiweek
  ) |>
  readr::write_csv(
    fs::path(
      output_path,
      "state_forecast_submission.csv"
    )
  )


cat(
  "\n============================================================\n",
  "PRODUCTION FORECAST COMPLETE\n",
  "============================================================\n",
  "Training end: ",
  max(
    train$epiweek
  ),
  " / ",
  as.character(
    max(
      train$date
    )
  ),
  "\n",
  "Forecast target: ",
  min(
    forecast$epiweek
  ),
  " to ",
  max(
    forecast$epiweek
  ),
  "\n",
  "Health regions: ",
  dplyr::n_distinct(
    forecast$regional_geocode
  ),
  "\n",
  "States: ",
  dplyr::n_distinct(
    submission$uf_code
  ),
  "\n",
  "Forecast weeks/state: 52\n",
  "Posterior predictive samples: ",
  n_samples,
  "\n",
  "Output directory: ",
  output_path,
  "\n",
  sep = ""
)
