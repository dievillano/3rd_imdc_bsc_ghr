get_split_metadata <- function(
    data,
    n_splits = 4,
    date_col = "date",
    epiweek_col = "epiweek"
) {
  
  purrr::map_dfr(seq_len(n_splits), \(i) {
    
    train_col <- paste0("train_", i)
    target_col <- paste0("target_", i)
    
    train_data <- data |> 
      dplyr::filter(.data[[train_col]])
    
    target_data <- data |> 
      dplyr::filter(.data[[target_col]])
    
    tibble::tibble(
      split_id = i,
      
      train_start_date = min(train_data[[date_col]], na.rm = TRUE),
      train_end_date   = max(train_data[[date_col]], na.rm = TRUE),
      train_start_epiweek = min(train_data[[epiweek_col]], na.rm = TRUE),
      train_end_epiweek   = max(train_data[[epiweek_col]], na.rm = TRUE),
      train_n_weeks = dplyr::n_distinct(train_data[[date_col]]),
      
      val_start_date = min(target_data[[date_col]], na.rm = TRUE),
      val_end_date   = max(target_data[[date_col]], na.rm = TRUE),
      val_start_epiweek = min(target_data[[epiweek_col]], na.rm = TRUE),
      val_end_epiweek   = max(target_data[[epiweek_col]], na.rm = TRUE),
      val_n_weeks = dplyr::n_distinct(target_data[[date_col]])
    )
  })
}