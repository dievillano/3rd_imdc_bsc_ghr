make_inla_vars <- function(data, climate_predictors) {
  for (v in climate_predictors) {
    data[[paste0(v, "_q10")]] <- INLA::inla.group(
      data[[v]],
      method = "quantile",
      n = 10
    )
    
    data[[paste0("koppen_id_", v)]] <- data$koppen_id
  }
  
  data
}