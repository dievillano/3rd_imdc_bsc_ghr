args <- commandArgs(trailingOnly = TRUE)

split_id <- as.integer(args[[1]])
model_id_arg <- args[[2]]

sprint2026_path <- here::here()

data_path <- fs::path(sprint2026_path, "data")
processed_data_path <- fs::path(data_path, "processed/health_region")
processed_graph_path <- fs::path(processed_data_path, "graph")
internal_splits_path <- fs::path(processed_data_path, "internal_splits")

dengue_path <- fs::path(sprint2026_path, "dengue/state")
hr_predictions_path <- fs::path(dengue_path, "outputs/cv_predictions")
state_predictions_path <- fs::path(dengue_path, "outputs/cv_predictions_state")

model_specs_path <- fs::path(dengue_path, "outputs/model_specs")

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

setup_hpc_library()

# 1. Read internal split --------------------------------------------------

internal_splits <- readRDS(
  fs::path(internal_splits_path, "dengue_hr_internal_splits.rds")
)

split <- internal_splits[[split_id]]

# 2. Scale split ----------------------------------------------------------

climate_predictors <- c(
  "temp_med_weight_rollmean_12_lag_8",
  "precip_min_weight_rollmean_12_lag_4",
  "precip_min_weight_anom_rollmean_12_lag_8",
  "rel_humid_max_weight_rollmean_8_lag_4",
  "rel_humid_min_weight_rollmean_24_lag_8",
  "rainy_days_weight_lag_8",
  "enso_lag_8"
)

scaled <- scale_split(
  train_data = split$train,
  validation_data = split$validation,
  predictors = tidyselect::all_of(climate_predictors),
  overwrite = TRUE
)

inla_vars <- make_inla_vars_split(
  train = scaled$train,
  validation = scaled$validation,
  predictors = climate_predictors
)

split_scaled <- list(
  train = inla_vars$train,
  validation = inla_vars$validation,
  scaler = scaled$scaler,
  metadata = split$metadata
)

# 3. Formula environment --------------------------------------------------

hr_graph <- INLA::inla.read.graph(
  fs::path(processed_graph_path, "hr_graph.graph")
)

prec_prior <- list(
  prec = list(prior = "pc.prec", param = c(0.5, 0.01))
)

bym2_prior <- list(
  prec = list(prior = "pc.prec", param = c(0.5, 0.01)),
  phi = list(prior = "pc", param = c(0.5, 2 / 3))
)

formula_env <- rlang::env(
  hr_graph = hr_graph,
  bym2_prior = bym2_prior,
  prec_prior = prec_prior
)

# 4. Run CV ---------------------------------------------------------------

cv_model_specs <- readRDS(fs::path(model_specs_path, "cv_model_specs.rds"))

model_spec <- cv_model_specs |>
  dplyr::filter(model_id == model_id_arg)

if (nrow(model_spec) != 1) {
  stop("Model ID not found or not unique: ", model_id_arg)
}

cat("Running split: ", split_id, "\n", sep = "")
cat("Running model: ", model_spec$model_label, "\n", sep = "")

out <- fit_forecast_inla(
  formula_string = model_spec$formula_string,
  train_data = split_scaled$train,
  forecast_data = split_scaled$validation,
  outcome = "cases",
  family = model_spec$family,
  offset = model_spec$offset,
  nthreads_inla = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")),
  forecast_ppd = TRUE,
  control_compute = list(config = TRUE),
  control_predictor = list(
    compute = TRUE,
    link = 1
  ),
  n_samples = 1000,
  formula_env = formula_env,
  split_metadata = split_scaled$metadata,
  keep_fit = FALSE,
  seed = 2026,
  verbose = TRUE
)

predictions <- out$predictions |>
  dplyr::mutate(
    .split_id = as.character(split_id),
    .model_id = model_spec$model_id,
    .model_label = model_spec$model_label
  ) |>
  dplyr::relocate(.split_id, .model_id, .model_label)

state_predictions <- predictions |>
  dplyr::group_by(
    .split_id, .model_id, .model_label,
    uf_code, date, epiweek, .lead_week
  ) |>
  dplyr::summarise(
    cases = sum(cases, na.rm = TRUE),
    .mu_mean = sum(.mu_mean, na.rm = TRUE),
    .pred_samples = list(purrr::reduce(.pred_samples, `+`)),
    .groups = "drop"
  )

hr_out_file <- fs::path(
  hr_predictions_path,
  paste0("split_", split_id, "_", model_spec$model_id, ".rds")
)

state_out_file <- fs::path(
  state_predictions_path,
  paste0("split_", split_id, "_", model_spec$model_id, ".rds")
)

saveRDS(predictions, hr_out_file)
saveRDS(state_predictions, state_out_file)

rm(out, predictions, state_predictions)
gc()

cat(
  "Saved:\n",
  hr_out_file, "\n",
  state_out_file, "\n",
  sep = ""
)