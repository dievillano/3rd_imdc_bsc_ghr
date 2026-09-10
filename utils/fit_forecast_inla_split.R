fit_forecast_inla_split <- function(
    split,
    split_id,
    formula_string,
    climate_predictors,
    formula_env,
    predictions_path,
    model_id = "previous_challenge_model",
    model_label = "Previous challenge model",
    n_samples = 1000,
    seed = 2026
) {
  
  scaled <- scale_split(
    train_data = split$train,
    validation_data = split$validation,
    predictors = tidyselect::all_of(climate_predictors),
    overwrite = TRUE
  )
  
  train_data <- add_model_vars(scaled$train)
  validation_data <- add_model_vars(scaled$validation)
  
  out <- fit_forecast_inla(
    formula_string = formula_string,
    train_data = train_data,
    forecast_data = validation_data,
    outcome = "cases",
    family = "nbinomial",
    exposure = "pop100k",
    offset = NULL,
    nthreads_inla = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")),
    forecast_ppd = TRUE,
    control_compute = list(config = TRUE),
    control_predictor = list(compute = TRUE, link = 1),
    n_samples = n_samples,
    formula_env = formula_env,
    split_metadata = split$metadata,
    keep_fit = FALSE,
    seed = seed,
    verbose = TRUE
  )
  
  predictions <- out$predictions |>
    dplyr::mutate(
      .split_id = as.character(split_id),
      .model_id = model_id,
      .model_label = model_label
    ) |>
    dplyr::relocate(.split_id, .model_id, .model_label)
  
  out_file <- fs::path(
    predictions_path,
    paste0("split_", split_id, "_", model_id, ".rds")
  )
  
  saveRDS(predictions, out_file)
  
  rm(out, predictions, train_data, validation_data, scaled)
  gc()
  
  invisible(out_file)
}