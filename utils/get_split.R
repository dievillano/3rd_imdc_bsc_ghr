get_split <- function(
    data,
    date_col,
    split_row,
    train_start_col = "train_start_date",
    train_end_col = "train_end_date",
    validation_start_col = "val_start_date",
    validation_end_col = "val_end_date"
) {
  
  train_data <- data |>
    dplyr::filter(
      dplyr::between(
        {{ date_col }},
        split_row[[train_start_col]],
        split_row[[train_end_col]]
      )
    )
  
  validation_data <- data |>
    dplyr::filter(
      dplyr::between(
        {{ date_col }},
        split_row[[validation_start_col]],
        split_row[[validation_end_col]]
      )
    )
  
  list(
    train = train_data,
    validation = validation_data,
    metadata = split_row
  )
}