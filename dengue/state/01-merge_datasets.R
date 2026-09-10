sprint2026_path <- here::here()

data_path <- fs::path(sprint2026_path, "data")
interim_data_path <- fs::path(data_path, "interim/health_region")
interim_data_filepaths <- fs::dir_ls(interim_data_path)
names(interim_data_filepaths) <- fs::path_file(interim_data_filepaths)

climate_path  <- fs::path(sprint2026_path, "climate")
climate_out_path  <- fs::path(climate_path, "out_files")

processed_data_path <- fs::path(data_path, "processed/health_region")
internal_splits_path <- fs::path(processed_data_path, "internal_splits")
external_splits_path <- fs::path(processed_data_path, "external_splits")

fs::dir_create(processed_data_path)
fs::dir_create(internal_splits_path)
fs::dir_create(external_splits_path)

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

# 1. Climate dataset ------------------------------------------------------

climate_raw <- readRDS(interim_data_filepaths["climate_hr.rds"])

climate_raw <- climate_raw |>
  dplyr::mutate(
    date = lubridate::ymd(paste0(year, "-", month, "-01"))
  )

# 1.1 Rolling averages and sums -------------------------------------------

roll_windows <- c(3, 6, 12)

tas_vars  <- c("tasmin", "tasmax", "tas")
prlr_vars <- "prlr"
nino_vars <- "nino"

climate <- climate_raw |>
  dplyr::arrange(regional_geocode, date) |>
  dplyr::group_by(regional_geocode)

for (w in roll_windows) {
  climate <- climate |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(tas_vars),
        \(x) slider::slide_dbl(
          x,
          \(y) mean(y, na.rm = TRUE),
          .before = w - 1,
          .complete = TRUE
        ),
        .names = paste0("{.col}", w)
      ),
      dplyr::across(
        tidyselect::all_of(prlr_vars),
        \(x) slider::slide_dbl(
          x,
          \(y) sum(y, na.rm = TRUE),
          .before = w - 1,
          .complete = TRUE
        ),
        .names = paste0("{.col}", w)
      ),
      dplyr::across(
        tidyselect::all_of(nino_vars),
        \(x) slider::slide_dbl(
          x,
          \(y) mean(y, na.rm = TRUE),
          .before = w - 1,
          .complete = TRUE
        ),
        .names = paste0("{.col}", w)
      )
    )
}

climate <- climate |>
  dplyr::ungroup()

# Nino year categories

nino_year <- climate |>
  dplyr::mutate(
    epiyear = dplyr::if_else(month < 10, year, year + 1L)
  ) |>
  dplyr::group_by(epiyear) |>
  dplyr::summarise(
    nino = mean(oni, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    nino_year = dplyr::case_when(
      nino < -0.5 ~ "Nina",
      nino >= -0.5 & nino <= 0.5 ~ "Neutral",
      nino > 0.5 ~ "Nino"
    ),
    nino_id = as.numeric(factor(nino_year))
  ) |>
  dplyr::select(epiyear, nino_year, nino_id)

# 1.2 Lags ----------------------------------------------------------------

id_vars <- c(
  "month", "year", "regional_geocode", "regional_name", "date"
)

lag_vars <- setdiff(names(climate), id_vars)

climate_lag <- climate |>
  dplyr::group_by(regional_geocode) |>
  dplyr::arrange(date, .by_group = TRUE) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(lag_vars),
      list(
        l1 = ~ dplyr::lag(.x, 1),
        l2 = ~ dplyr::lag(.x, 2),
        l3 = ~ dplyr::lag(.x, 3),
        l4 = ~ dplyr::lag(.x, 4),
        l5 = ~ dplyr::lag(.x, 5),
        l6 = ~ dplyr::lag(.x, 6)
      ),
      .names = "{.col}.{.fn}"
    )
  ) |>
  dplyr::ungroup() |> 
  dplyr::select(-date)

# 3. Environmental dataset ------------------------------------------------

environ <- readRDS(interim_data_filepaths["environ_hr.rds"])

# 4. Population dataset ---------------------------------------------------

population <- readRDS(interim_data_filepaths["population_hr.rds"])

# 5. Dengue dataset -------------------------------------------------------

dengue_raw <- readRDS(interim_data_filepaths["dengue_hr.rds"])

last_date <- as.Date("2026-10-04")  # Sunday of EW40 2026

future_dates <- seq(
  from = max(dengue_raw$date) + 7,
  to = last_date,
  by = "week"
)

future_epiweeks <- seq(
  from = max(dengue_raw$epiweek) + 1L,
  to = 202640L,
  by = 1L
)

stopifnot(length(future_dates) == length(future_epiweeks))

ids <- dengue_raw |>
  dplyr::distinct(
    uf_code,
    uf,
    regional_geocode
  )

future_rows <- tidyr::expand_grid(
  ids,
  tibble::tibble(
    date = future_dates,
    epiweek = future_epiweeks
  )
) |>
  dplyr::mutate(
    cases = NA_real_,

    train_1 = FALSE,
    train_2 = FALSE,
    train_3 = FALSE,
    train_4 = FALSE,

    target_1 = FALSE,
    target_2 = FALSE,
    target_3 = FALSE,
    target_4 = TRUE
  ) |>
  dplyr::select(dplyr::all_of(names(dengue_raw)))

dengue <- dplyr::bind_rows(
  dengue_raw,
  future_rows
) |>
  dplyr::mutate(
    month = lubridate::month(date + 3),
    year = lubridate::year(date + 3)
  ) |>
  dplyr::arrange(
    regional_geocode,
    date
  )

# 6. Merge datasets -------------------------------------------------------

data <- dengue |>
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
  )

rm(climate_raw, climate, climate_lag)
gc()

data <- data |>
  dplyr::arrange(regional_geocode, date) |>
  dplyr::mutate(
    hr_id = as.integer(factor(regional_geocode)),
    state_id = as.integer(factor(uf_code)),
    time_id = as.integer(factor(epiweek)),
    epiweek_num = as.integer(substr(epiweek, 5, 6)),
    epiweek_num = dplyr::if_else(epiweek_num == 53L, 52L, epiweek_num),
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
    year_id = as.integer(factor(epiyear)),
    koppen = factor(koppen),
    koppen_id = as.integer(koppen),
    biome = factor(biome),
    biome_id = as.integer(biome),
    pop100k = population / 100000
  )

validate_weekly_panel(
  data = data,
  unit = regional_geocode,
  date = date,
  epiweek = epiweek
)

# 7. Get validation splits ------------------------------------------------

forecast_files <- c(
  validation_1 = "climate_forecasts_val_1.rds",
  validation_2 = "climate_forecasts_val_2.rds",
  validation_3 = "climate_forecasts_val_3.rds",
  validation_4 = "climate_forecasts_val_4.rds"
)

forecast_lookup <- tibble::tibble(
  split_id = 1:4,
  forecast_file = unname(forecast_files)
)

split_metadata <- get_split_metadata(data) |>
  dplyr::left_join(forecast_lookup, by = "split_id")

external_splits <- split_metadata |>
  (\(x) split(x, x$split_id))() |>
  purrr::map(
    \(split_row) {
      make_external_split(
        data_obs = data,
        forecast_file = split_row$forecast_file,
        split_row = dplyr::select(split_row, -forecast_file)
      )
    }
  )

saveRDS(
  external_splits,
  fs::path(
    external_splits_path,
    "dengue_hr_external_splits.rds"
  )
)

internal_split_plan <- make_internal_split_plan(
  data = data,
  cutoff_years = c(2019, 2020, 2021)
)

internal_splits <- internal_split_plan |>
  split(seq_len(nrow(internal_split_plan))) |>
  purrr::map(
    \(split_row) {
      make_internal_split(
        data_obs = data,
        split_row = split_row
      )
    }
  )

saveRDS(
  internal_splits,
  fs::path(internal_splits_path, "dengue_hr_internal_splits.rds")
)

saveRDS(
  data,
  fs::path(processed_data_path, "dengue_hr_full.rds")
)

saveRDS(
  split_metadata,
  fs::path(processed_data_path, "dengue_hr_split_metadata.rds")
)


