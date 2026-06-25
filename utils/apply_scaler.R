apply_scaler <- function(data, scaler, suffix = "_scaled", overwrite = FALSE) {
  
  missing_predictors <- setdiff(scaler$predictor, names(data))
  
  if (length(missing_predictors) > 0) {
    stop(
      "The following predictors are missing from `data`: ",
      paste(missing_predictors, collapse = ", ")
    )
  }
  
  data_std <- data
  
  for (i in seq_len(nrow(scaler))) {
    var <- scaler$predictor[i]
    mu  <- scaler$mean[i]
    sig <- scaler$sd[i]
    
    if (is.na(sig) || sig == 0) {
      stop("Predictor '", var, "' has zero or missing SD in the training data.")
    }
    
    new_var <- if (overwrite) var else paste0(var, suffix)
    
    data_std[[new_var]] <- (data_std[[var]] - mu) / sig
  }
  
  data_std
}