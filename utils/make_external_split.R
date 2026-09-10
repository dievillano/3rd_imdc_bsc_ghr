make_external_split <- function(data_obs, forecast_file, split_row) {
  
  climate_forecast <- readRDS(
    fs::path(interim_data_path, forecast_file)
  ) 
  
  climate_keys <- c("regional_geocode", "month", "year", "date")
  climate_vars <- setdiff(names(climate_forecast), climate_keys)
  
  data_fcst <- data_obs |>
    dplyr::select(-tidyselect::any_of(climate_vars)) |>
    dplyr::left_join(
      climate_forecast,
      by = c("regional_geocode", "month", "year")
    )
  
  split_obs <- get_split(
    data = data_obs,
    date_col = date,
    split_row = split_row
  )
  
  split_fcst <- get_split(
    data = data_fcst,
    date_col = date,
    split_row = split_row
  )
  
  list(
    train = split_obs$train,
    validation = split_fcst$validation,
    validation_obs = split_obs$validation,
    metadata = split_obs$metadata
  )
}