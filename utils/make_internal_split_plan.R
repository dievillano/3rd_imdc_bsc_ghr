make_internal_split_plan <- function(data, cutoff_years) {
  purrr::map_dfr(cutoff_years, \(yr) {
    tibble::tibble(
      split_id = paste0("internal_", yr),
      train_start_date = min(data$date, na.rm = TRUE),
      train_end_date = data |>
        dplyr::filter(epiweek == as.integer(paste0(yr, "25"))) |>
        dplyr::summarise(date = max(date, na.rm = TRUE)) |>
        dplyr::pull(date),
      val_start_date = data |>
        dplyr::filter(epiweek == as.integer(paste0(yr, "41"))) |>
        dplyr::summarise(date = min(date, na.rm = TRUE)) |>
        dplyr::pull(date),
      val_end_date = data |>
        dplyr::filter(epiweek == as.integer(paste0(yr + 1, "40"))) |>
        dplyr::summarise(date = max(date, na.rm = TRUE)) |>
        dplyr::pull(date)
    )
  })
}