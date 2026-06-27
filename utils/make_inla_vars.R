make_inla_vars_split <- function(train, validation, predictors) {
  
  n_train <- nrow(train)
  
  combined <- dplyr::bind_rows(
    train |> dplyr::mutate(.row_type_inla = "train"),
    validation |> dplyr::mutate(.row_type_inla = "validation")
  )
  
  for (v in predictors) {
    combined[[paste0(v, "_q10")]] <- INLA::inla.group(
      combined[[v]],
      method = "quantile",
      n = 10
    )
    
    combined[[paste0("koppen_id_", v)]] <- combined$koppen_id
  }
  
  list(
    train = combined |>
      dplyr::slice(seq_len(n_train)) |>
      dplyr::select(-.row_type_inla),
    
    validation = combined |>
      dplyr::slice((n_train + 1):dplyr::n()) |>
      dplyr::select(-.row_type_inla)
  )
}