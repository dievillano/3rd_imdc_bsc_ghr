fit_forecast_inla <- function(
    formula_string,
    train_data,
    forecast_data,
    outcome = "cases",
    family = "nbinomial",
    offset = NULL,
    nthreads_inla = 1,
    forecast_ppd = FALSE,
    control_compute = list(),
    control_predictor = list(),
    n_samples = 1000,
    formula_env = parent.frame(),
    split_metadata = NULL,
    date_col = "date",
    keep_fit = TRUE,
    seed = NULL,
    ...
) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # 1. Prepare model data --------------------------------------------------
  
  train_data <- train_data |>
    dplyr::mutate(.row_type = "train")
  
  forecast_data <- forecast_data |>
    dplyr::mutate(
      .row_type = "forecast",
      .observed = .data[[outcome]]
    )
  
  # Hide forecast outcomes from INLA
  forecast_data[[outcome]] <- NA
  
  model_data <- dplyr::bind_rows(
    train_data,
    forecast_data
  ) |> 
    dplyr::mutate(.row_id = dplyr::row_number())
  
  # 2. Control compute -----------------------------------------------------
  
  if (forecast_ppd) {
    control_compute <- utils::modifyList(control_compute, list(config = TRUE))
    control_predictor <- utils::modifyList(
      control_predictor,
      list(compute = TRUE, link = 1)
    )
  }
  
  # 3. Fit model -----------------------------------------------------------
  
  fit <- fit_one_inla(
    formula_string = formula_string,
    data = model_data,
    family = family,
    offset = offset,
    nthreads_inla = nthreads_inla,
    control_compute = control_compute,
    control_predictor = control_predictor,
    formula_env = formula_env,
    ...
  )
  
  # 4. Extract fitted values ----------------------------------------------
  
  fitted <- fit$summary.fitted.values
  
  pred <- model_data |>
    dplyr::mutate(
      .mu_mean = fitted$mean,
      .mu_sd = fitted$sd,
      .mu_q025 = fitted$`0.025quant`,
      .mu_q500 = fitted$`0.5quant`,
      .mu_q975 = fitted$`0.975quant`
    ) |>
    dplyr::filter(.row_type == "forecast")
  
  pred[[outcome]] <- pred$.observed
  pred <- pred |> 
    dplyr::select(-.observed)
  
  # 5. Add split metadata / lead month ------------------------------------
  
  if (!is.null(split_metadata)) {
    
    if (!date_col %in% names(pred)) {
      stop("`date_col` was not found in the prediction data.")
    }
    
    pred <- pred |>
      dplyr::mutate(
        .split_id = split_metadata$split_id,
        .train_start_date = split_metadata$train_start_date,
        .train_end_date = split_metadata$train_end_date,
        .forecast_start_date = split_metadata$val_start_date,
        .forecast_end_date = split_metadata$val_end_date
      ) |>
      dplyr::arrange(.data[[date_col]]) |>
      dplyr::mutate(
        .lead_week = dplyr::dense_rank(.data[[date_col]])
      )
  }
  
  # 6. Posterior predictive samples ---------------------------------------
  
  if (forecast_ppd) {
    
    forecast_idx <- model_data |>
      dplyr::filter(.row_type == "forecast") |>
      dplyr::pull(.row_id)
    
    fit_samples <- INLA::inla.posterior.sample(
      n = n_samples,
      result = fit,
      selection = list(Predictor = forecast_idx)
    )
    
    eta_samples <- do.call(
      rbind,
      purrr::map(fit_samples, \(x) as.vector(x$latent))
    )
    
    mu_samples <- exp(eta_samples)
    mu_samples <- pmax(mu_samples, 0)
    
    stopifnot(nrow(mu_samples) == n_samples)
    stopifnot(ncol(mu_samples) == length(forecast_idx))
    
    colnames(mu_samples) <- as.character(forecast_idx)
    rownames(mu_samples) <- paste0(".sample_", seq_len(n_samples))
    
    if (family == "poisson") {
      
      pred_samples <- apply(
        mu_samples,
        2,
        \(mu) stats::rpois(
          n = n_samples,
          lambda = mu
        )
      )
      
    } else if (family == "nbinomial") {
      
      size_samples <- purrr::map_dbl(
        fit_samples,
        \(x) {
          size_id <- grep("size", names(x$hyperpar))
          
          if (length(size_id) != 1) {
            stop("Could not uniquely identify the negative-binomial size parameter.")
          }
          
          x$hyperpar[[size_id]]
        }
      )
      
      pred_samples <- apply(
        mu_samples,
        2,
        \(mu) stats::rnbinom(
          n = n_samples,
          size = size_samples,
          mu = mu
        )
      )
      
    } else {
      stop(
        "Posterior predictive sampling not implemented for family: ",
        family
      )
    }
    
    stopifnot(nrow(pred_samples) == n_samples)
    stopifnot(ncol(pred_samples) == length(forecast_idx))
    
    colnames(pred_samples) <- as.character(forecast_idx)
    rownames(pred_samples) <- paste0(".sample_", seq_len(n_samples))
    
    pred_samples_df <- tibble::tibble(
      .row_id = forecast_idx,
      .mu_samples = purrr::map(
        as.character(forecast_idx),
        \(x) mu_samples[, x]
      ),
      .pred_samples = purrr::map(
        as.character(forecast_idx),
        \(x) pred_samples[, x]
      )
    )
    
    pred <- pred |>
      dplyr::left_join(pred_samples_df, by = ".row_id")
  }
  
  # 7. Return --------------------------------------------------------------
  
  out <- list(
    predictions = pred
  )
  
  if (keep_fit) {
    out$fit <- fit
  }
  
  out
}
  
  