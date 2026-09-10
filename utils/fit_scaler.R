fit_scaler <- function(data, ...) {
  predictors <- names(dplyr::select(data, ...))
  
  data |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(predictors),
        list(
          mean = \(x) mean(x, na.rm = TRUE),
          sd   = \(x) stats::sd(x, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      )
    ) |> 
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("predictor", ".value"),
      names_sep = "__"
    )
}