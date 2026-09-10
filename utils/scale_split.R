scale_split <- function(
    train_data,
    validation_data = NULL,
    predictors,
    suffix = "_scaled",
    overwrite = FALSE
) {
  scaler <- fit_scaler(train_data, {{ predictors }})
  
  if (!is.null(validation_data)) {
    list(
      train = apply_scaler(
        train_data, scaler, suffix = suffix, overwrite = overwrite
      ),
      validation = apply_scaler(
        validation_data, scaler, suffix = suffix, overwrite = overwrite
      ),
      scaler = scaler
    )
  } else {
    list(
      train = apply_scaler(
        train_data, scaler, suffix = suffix, overwrite = overwrite
      ),
      scaler = scaler
    )
  }
}

# std_split <- standardise_split(
#   train_data = split$train,
#   validation_data = split$validation,
#   starts_with("tas_"),
#   starts_with("prlr_"),
#   contains("oni"),
#   suffix = "_std"
# )
# 
# train_std <- std_split$train
# validation_std <- std_split$validation
# scaler <- std_split$scaler

