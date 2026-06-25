run_cv_inla <- function(
    model_specs,
    splits,
    outcome = "cases",
    family = "nbinomial",
    offset = "pop100k",
    nthreads_inla = 1,
    forecast_ppd = TRUE,
    control_compute = list(),
    control_predictor = list(),
    formula_env = parent.frame(),
    keep_fit = FALSE,
    n_samples = 1000,
    seed = 123,
    verbose = TRUE,
    ...
) {
  
  results <- purrr::imap(
    splits, 
    \(split, split_id) {
      
      if (verbose) {
        message("Running split: ", split_id)
      }
      
      model_specs_fit <- model_specs |>
        dplyr::select(model_id, model_label, formula_string)
      
      purrr::pmap(
        model_specs_fit,
        \(model_id, model_label, formula_string) {
          
          if (verbose) {
            message("  Fitting model: ", model_label)
          }
          
          out <- fit_forecast_inla(
            formula_string = formula_string,
            train_data = split$train,
            forecast_data = split$validation,
            outcome = outcome,
            family = family,
            offset = offset,
            nthreads_inla = nthreads_inla,
            forecast_ppd = forecast_ppd,
            control_compute = control_compute,
            control_predictor = control_predictor,
            n_samples = n_samples,
            formula_env = formula_env,
            split_metadata = split$metadata,
            keep_fit = keep_fit,
            seed = seed,
            ...
          )
          
          out$predictions <- out$predictions |>
            dplyr::mutate(
              .split_id = split_id,
              .model_id = model_id,
              .model_label = model_label
            ) |> 
            dplyr::relocate(.split_id, .model_id, .model_label)
          
          out
        }
      ) |>
        rlang::set_names(model_specs$model_id)
    }
  )
  
  results
}