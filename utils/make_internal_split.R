make_internal_split <- function(data_obs, split_row) {
  
  split_obs <- get_split(
    data = data_obs,
    date_col = date,
    split_row = split_row
  )
  
  list(
    train = split_obs$train,
    validation = split_obs$validation,
    validation_obs = split_obs$validation,
    metadata = split_obs$metadata
  )
}