sprint2026_path <- here::here()
data_path <- fs::path(sprint2026_path, "data")
interim_data_path <- fs::path(data_path, "interim/health_region")
interim_data_filepaths <- fs::dir_ls(interim_data_path)
names(interim_data_filepaths) <- fs::path_file(interim_data_filepaths)

processed_data_path <- fs::path(data_path, "processed/health_region")
internal_splits_path <- fs::path(processed_data_path, "internal_splits")
external_splits_path <- fs::path(processed_data_path, "external_splits")

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

# 1. Climate dataset ------------------------------------------------------

climate_raw <- readRDS(interim_data_filepaths["climate_hr.rds"])

# 1.1 Rolling averages and sums -------------------------------------------

climate_colnames <- colnames(climate_raw)

rm_vars <- climate_colnames[
  grepl("^(temp|precip|rel_humid)", climate_colnames)
]

roll_windows <- c(4, 8, 12, 24)

climate <- climate_raw |> 
  dplyr::arrange(regional_geocode, date) |> 
  dplyr::group_by(regional_geocode)

for (w in roll_windows) {
  climate <- climate |> 
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(rm_vars),
        \(x) slider::slide_dbl(
          x, 
          mean, 
          .before = w - 1, 
          .complete = TRUE
        ),
        .names = paste0("{.col}_rollmean_", w)
      )
    )
}

climate <- climate |>
  dplyr::ungroup()

# 1.2 Lags ----------------------------------------------------------------

raw_vars <- grep(
  "^(temp|precip|rel_humid).*_weight$", 
  names(climate), 
  value = TRUE
)

anom_vars <- grep(
  "^(temp|precip|rel_humid).*_anom$",
  names(climate),
  value = TRUE
)

rolling_vars <- grep(
  "^(temp|precip|rel_humid).*_(rollmean)_",
  names(climate),
  value = TRUE
)

secondary_vars <- c(
  "thermal_range_weight", 
  "rainy_days_weight"
)

raw_lags <- c(4, 8, 12)
anom_lags <- c(4, 8, 12)
roll_lags <- c(4, 8)
secondary_lags <- c(4, 8)

climate_lag <- GHRmodel::lag_cov(
  data = climate,
  name = raw_vars,
  time = "date",
  group = "regional_geocode",
  lag = raw_lags
)

climate_lag <- GHRmodel::lag_cov(
  data = climate_lag,
  name = anom_vars,
  time = "date",
  group = "regional_geocode",
  lag = anom_lags
)

climate_lag <- GHRmodel::lag_cov(
  data = climate_lag,
  name = rolling_vars,
  time = "date",
  group = "regional_geocode",
  lag = roll_lags
)

climate_lag <- GHRmodel::lag_cov(
  data = climate_lag,
  name = secondary_vars,
  time = "date",
  group = "regional_geocode",
  lag = secondary_lags
)

climate_lag_cols <- grep("\\.l[0-9]+$", names(climate_lag), value = TRUE)

climate_lag <- climate_lag |> 
  dplyr::rename_with(
    ~ gsub("\\.l([0-9]+)$", "_lag_\\1", .x),
    .cols = tidyselect::all_of(climate_lag_cols)
  )

# 2. Ocean dataset --------------------------------------------------------

ocean_raw <- readRDS(interim_data_filepaths["ocean.rds"])

ocean_lags <- c(4, 8, 12, 24)

ocean_vars <- c("enso", "iod", "pdo")

ocean_lag <- GHRmodel::lag_cov(
  data = ocean_raw,
  name = ocean_vars,
  time = "date",
  lag = ocean_lags
)

ocean_lag_cols <- grep("\\.l[0-9]+$", names(ocean_lag), value = TRUE)

ocean_lag <- ocean_lag |> 
  dplyr::rename_with(
    ~ gsub("\\.l([0-9]+)$", "_lag_\\1", .x),
    .cols = tidyselect::all_of(ocean_lag_cols)
  )

# 3. Environmental dataset ------------------------------------------------

environ <- readRDS(interim_data_filepaths["environ_hr.rds"])

# 4. Population dataset ---------------------------------------------------

population <- readRDS(interim_data_filepaths["population_hr.rds"])

# 5. Dengue dataset -------------------------------------------------------

dengue <- readRDS(interim_data_filepaths["dengue_hr.rds"])

# 6. Merge datasets -------------------------------------------------------

data <- dengue |> 
  dplyr::left_join(
    climate_lag, 
    by = c(
      "uf_code",
      "uf",
      "regional_geocode",
      "date",
      "epiweek"
    )
  ) |> 
  dplyr::left_join(
    ocean_lag,
    by = c(
      "date",
      "epiweek"
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
    
    # Spatial IDs
    hr_id = as.integer(factor(regional_geocode)),
    state_id = as.integer(factor(uf_code)),
    
    # Temporal IDs
    time_id = as.integer(factor(epiweek)),
    
    # Week within dengue season: EW41 = 1, ..., EW40 = 52
    week_id = dplyr::if_else(
      epiweek_num >= 41L,
      epiweek_num - 40L,
      epiweek_num + 12L
    ),
    
    # Calendar month/year assigned to the middle of the epiweek
    month = lubridate::month(date + 3),
    month_id = month,
    year = lubridate::year(date + 3),
    
    # Dengue season year: EW41 2022 - EW40 2023 becomes epiyear 2023
    epiyear = dplyr::if_else(
      epiweek_num <= 40L,
      year,
      year + 1L
    ),
    year_id = as.integer(factor(epiyear)),
    
    # Environmental factors and IDs
    koppen = factor(koppen),
    koppen_id = as.integer(koppen),
    biome = factor(biome),
    biome_id = as.integer(biome),
    
    # Population
    pop100k = population / 100000
  )

validate_weekly_panel(
  data = data,
  unit = regional_geocode,
  date = date,
  epiweek = epiweek
)

# 7. Get validation splits ------------------------------------------------

split_metadata <- get_split_metadata(data)
split_metadata

external_splits <- split_metadata |>
  split(seq_len(nrow(split_metadata))) |>
  purrr::map(
    \(x) get_split(
      data = data,
      date_col = date,
      split_row = x
    )
  )

saveRDS(
  external_splits,
  fs::path(external_splits_path, "dengue_hr_external_splits.rds")
)

rm(external_splits)
gc()

internal_split_plan <- make_internal_split_plan(
  data = data,
  cutoff_years = c(2019, 2020, 2021)
)

internal_splits <- internal_split_plan |>
  split(seq_len(nrow(internal_split_plan))) |>
  purrr::map(
    \(x) get_split(
      data = data,
      date_col = date,
      split_row = x
    )
  )

saveRDS(
  internal_splits,
  fs::path(internal_splits_path, "dengue_hr_internal_splits.rds")
)

rm(internal_splits)
gc()

saveRDS(
  data,
  fs::path(processed_data_path, "dengue_hr_full.rds")
)

saveRDS(
  split_metadata,
  fs::path(processed_data_path, "dengue_hr_split_metadata.rds")
)


