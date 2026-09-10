fit_forecast_inla <- function(
    formula_string,
    train_data,
    forecast_data,
    outcome = "cases",
    family = "nbinomial",
    exposure = NULL,
    offset = NULL,
    nthreads_inla = 1L,
    forecast_ppd = FALSE,
    control_compute = list(),
    control_predictor = list(),
    n_samples = 1000L,
    formula_env = parent.frame(),
    split_metadata = NULL,
    date_col = "date",
    keep_fit = TRUE,
    seed = NULL,
    ...
) {
  
  # 1. Validate inputs -----------------------------------------------------
  
  if (!is.data.frame(train_data)) {
    rlang::abort(
      "`train_data` must be a data frame."
    )
  }
  
  if (!is.data.frame(forecast_data)) {
    rlang::abort(
      "`forecast_data` must be a data frame."
    )
  }
  
  if (!outcome %in% names(train_data)) {
    rlang::abort(
      "`outcome` was not found in `train_data`."
    )
  }
  
  if (!outcome %in% names(forecast_data)) {
    rlang::abort(
      "`outcome` was not found in `forecast_data`."
    )
  }
  
  
  # Exposure corresponds to INLA's E argument.
  
  if (!is.null(exposure)) {
    
    if (
      !is.character(exposure) ||
      length(exposure) != 1L ||
      is.na(exposure)
    ) {
      rlang::abort(
        "`exposure` must be NULL or one column name."
      )
    }
    
    if (
      !exposure %in% names(train_data) ||
      !exposure %in% names(forecast_data)
    ) {
      rlang::abort(
        "`exposure` was not found in both train and forecast data."
      )
    }
  }
  
  
  # Offset corresponds to INLA's additive linear-predictor offset.
  
  if (!is.null(offset)) {
    
    if (
      !is.character(offset) ||
      length(offset) != 1L ||
      is.na(offset)
    ) {
      rlang::abort(
        "`offset` must be NULL or one column name."
      )
    }
    
    if (
      !offset %in% names(train_data) ||
      !offset %in% names(forecast_data)
    ) {
      rlang::abort(
        "`offset` was not found in both train and forecast data."
      )
    }
  }
  
  
  if (
    !is.null(exposure) &&
    !is.null(offset) &&
    identical(exposure, offset)
  ) {
    rlang::abort(
      paste(
        "`exposure` and `offset` refer to the same column.",
        "They represent different model components."
      )
    )
  }
  
  
  if (
    length(n_samples) != 1L ||
    !is.numeric(n_samples) ||
    is.na(n_samples) ||
    n_samples < 1L ||
    n_samples != as.integer(n_samples)
  ) {
    rlang::abort(
      "`n_samples` must be one positive integer."
    )
  }
  
  n_samples <- as.integer(
    n_samples
  )
  
  
  if (
    !is.null(seed) &&
    (
      length(seed) != 1L ||
      !is.numeric(seed) ||
      is.na(seed)
    )
  ) {
    rlang::abort(
      "`seed` must be NULL or one non-missing numeric value."
    )
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  
  # 2. Prepare model data --------------------------------------------------
  
  train_data <- train_data |>
    dplyr::mutate(
      .row_type = "train"
    )
  
  forecast_data <- forecast_data |>
    dplyr::mutate(
      .row_type = "forecast",
      .observed = .data[[outcome]]
    )
  
  
  # Hide forecast outcomes from INLA.
  
  forecast_data[[outcome]] <- NA
  
  
  model_data <- dplyr::bind_rows(
    train_data,
    forecast_data
  ) |>
    dplyr::mutate(
      .row_id = dplyr::row_number()
    )
  
  
  # 3. Validate exposure and offset values --------------------------------
  
  if (!is.null(exposure)) {
    
    exposure_values <- model_data[[exposure]]
    
    if (!is.numeric(exposure_values)) {
      rlang::abort(
        "`exposure` must refer to a numeric column."
      )
    }
    
    if (
      anyNA(exposure_values) ||
      any(!is.finite(exposure_values)) ||
      any(exposure_values <= 0)
    ) {
      rlang::abort(
        paste(
          "`exposure` must contain only",
          "positive finite values."
        )
      )
    }
    
  } else {
    
    exposure_values <- rep(
      1,
      nrow(model_data)
    )
  }
  
  
  if (!is.null(offset)) {
    
    offset_values <- model_data[[offset]]
    
    if (!is.numeric(offset_values)) {
      rlang::abort(
        "`offset` must refer to a numeric column."
      )
    }
    
    if (
      anyNA(offset_values) ||
      any(!is.finite(offset_values))
    ) {
      rlang::abort(
        "`offset` must contain only finite values."
      )
    }
  }
  
  
  # 4. Configure INLA ------------------------------------------------------
  
  if (forecast_ppd) {
    
    control_compute <- utils::modifyList(
      control_compute,
      list(
        config = TRUE
      )
    )
    
    control_predictor <- utils::modifyList(
      control_predictor,
      list(
        compute = TRUE,
        link = 1
      )
    )
  }
  
  
  # 5. Fit model -----------------------------------------------------------
  
  fit <- fit_one_inla(
    formula_string = formula_string,
    data = model_data,
    family = family,
    exposure = exposure,
    offset = offset,
    nthreads_inla = nthreads_inla,
    control_compute = control_compute,
    control_predictor = control_predictor,
    formula_env = formula_env,
    ...
  )
  
  
  # 6. Extract conditional fitted means -----------------------------------
  #
  # `summary.fitted.values` already incorporates a conventional INLA
  # offset because that offset is part of the predictor.
  #
  # It does NOT incorporate E, so exposure must be applied explicitly.
  #
  # Therefore `.mu_*` always represents the conditional mean on the
  # response scale.
  
  fitted <- fit$summary.fitted.values
  
  if (nrow(fitted) != nrow(model_data)) {
    rlang::abort(
      paste(
        "The number of fitted values does not match",
        "the number of rows in `model_data`."
      )
    )
  }
  
  
  pred <- model_data |>
    dplyr::mutate(
      .mu_mean =
        fitted$mean *
        exposure_values,
      
      .mu_sd =
        fitted$sd *
        exposure_values,
      
      .mu_q025 =
        fitted$`0.025quant` *
        exposure_values,
      
      .mu_q500 =
        fitted$`0.5quant` *
        exposure_values,
      
      .mu_q975 =
        fitted$`0.975quant` *
        exposure_values
    ) |>
    dplyr::filter(
      .row_type == "forecast"
    )
  
  
  pred[[outcome]] <- pred$.observed
  
  pred <- pred |>
    dplyr::select(
      -.observed
    )
  
  
  # 7. Add split metadata / forecast lead ---------------------------------
  
  if (!is.null(split_metadata)) {
    
    pred <- append_split_metadata(
      data = pred,
      metadata = split_metadata,
      prefix = "."
    )
  }
  
  
  if (!".lead_month" %in% names(pred)) {
    
    if (!date_col %in% names(pred)) {
      rlang::abort(
        "`date_col` was not found in the prediction data."
      )
    }
    
    pred <- pred |>
      dplyr::mutate(
        .lead_month = dplyr::dense_rank(
          .data[[date_col]]
        )
      )
  }
  
  
  # 8. Posterior predictive samples ---------------------------------------
  
  if (forecast_ppd) {
    
    forecast_idx <- model_data |>
      dplyr::filter(
        .row_type == "forecast"
      ) |>
      dplyr::pull(
        .row_id
      )
    
    n_forecast <- length(
      forecast_idx
    )
    
    
    if (n_forecast == 0L) {
      rlang::abort(
        "No forecast rows were found."
      )
    }
    
    
    # 8.1 Draw from joint posterior ----------------------------------------
    
    fit_samples <- INLA::inla.posterior.sample(
      n = n_samples,
      result = fit,
      selection = list(
        Predictor = forecast_idx
      )
    )
    
    
    if (length(fit_samples) != n_samples) {
      rlang::abort(
        "INLA returned an unexpected number of posterior samples."
      )
    }
    
    
    # Rows = posterior samples
    # Columns = forecast observations
    #
    # The sampled Predictor already contains any INLA offset.
    # It does not contain E.
    
    eta_samples <- do.call(
      rbind,
      purrr::map(
        fit_samples,
        \(sample) {
          as.vector(
            sample$latent
          )
        }
      )
    )
    
    
    if (
      nrow(eta_samples) != n_samples ||
      ncol(eta_samples) != n_forecast
    ) {
      rlang::abort(
        paste(
          "Unexpected dimensions for posterior",
          "linear-predictor samples."
        )
      )
    }
    
    
    # 8.2 Convert predictor -> conditional mean ----------------------------
    #
    # For the currently supported count likelihoods:
    #
    #   no exposure:
    #     mu = exp(eta)
    #
    #   exposure E:
    #     mu = E * exp(eta)
    #
    # Any conventional offset is already contained in eta.
    
    mu_samples <- exp(
      eta_samples
    )
    
    
    if (!is.null(exposure)) {
      
      forecast_exposure <- exposure_values[
        forecast_idx
      ]
      
      mu_samples <- sweep(
        mu_samples,
        MARGIN = 2,
        STATS = forecast_exposure,
        FUN = "*"
      )
    }
    
    
    if (
      anyNA(mu_samples) ||
      any(!is.finite(mu_samples)) ||
      any(mu_samples < 0)
    ) {
      rlang::abort(
        "Invalid conditional-mean samples were generated."
      )
    }
    
    
    colnames(mu_samples) <- as.character(
      forecast_idx
    )
    
    rownames(mu_samples) <- paste0(
      ".sample_",
      seq_len(n_samples)
    )
    
    
    # 8.3 Simulate observation likelihood ---------------------------------
    
    if (family == "poisson") {
      
      pred_samples <- vapply(
        seq_len(n_forecast),
        \(j) {
          
          stats::rpois(
            n = n_samples,
            lambda = mu_samples[, j]
          )
        },
        numeric(n_samples)
      )
      
      
    } else if (family == "nbinomial") {
      
      size_samples <- purrr::map_dbl(
        fit_samples,
        \(sample) {
          
          size_id <- grep(
            "size",
            names(sample$hyperpar),
            ignore.case = TRUE
          )
          
          
          if (length(size_id) != 1L) {
            rlang::abort(
              paste(
                "Could not uniquely identify the",
                "negative-binomial size parameter."
              )
            )
          }
          
          
          sample$hyperpar[[size_id]]
        }
      )
      
      
      if (
        anyNA(size_samples) ||
        any(!is.finite(size_samples)) ||
        any(size_samples <= 0)
      ) {
        rlang::abort(
          paste(
            "Invalid negative-binomial size",
            "posterior samples were generated."
          )
        )
      }
      
      
      pred_samples <- vapply(
        seq_len(n_forecast),
        \(j) {
          
          stats::rnbinom(
            n = n_samples,
            size = size_samples,
            mu = mu_samples[, j]
          )
        },
        numeric(n_samples)
      )
      
      
    } else {
      
      rlang::abort(
        paste0(
          "Posterior predictive sampling is not implemented for family: ",
          family
        )
      )
    }
    
    
    if (
      nrow(pred_samples) != n_samples ||
      ncol(pred_samples) != n_forecast
    ) {
      rlang::abort(
        "Unexpected dimensions for posterior predictive samples."
      )
    }
    
    
    colnames(pred_samples) <- as.character(
      forecast_idx
    )
    
    rownames(pred_samples) <- paste0(
      ".sample_",
      seq_len(n_samples)
    )
    
    
    # 8.4 Attach samples ---------------------------------------------------
    
    pred_samples_df <- tibble::tibble(
      .row_id = forecast_idx,
      
      .mu_samples = purrr::map(
        as.character(forecast_idx),
        \(current_id) {
          mu_samples[, current_id]
        }
      ),
      
      .pred_samples = purrr::map(
        as.character(forecast_idx),
        \(current_id) {
          pred_samples[, current_id]
        }
      )
    )
    
    
    pred <- pred |>
      dplyr::left_join(
        pred_samples_df,
        by = ".row_id"
      )
    
    
    # 8.5 Internal consistency check --------------------------------------
    #
    # `.mu_mean` is now on the same response scale as `.mu_samples`,
    # regardless of whether E is used.
    
    sampled_mu_mean <- colMeans(
      mu_samples
    )
    
    
    if (
      any(!is.finite(sampled_mu_mean)) ||
      any(!is.finite(pred$.mu_mean))
    ) {
      rlang::abort(
        paste(
          "Non-finite conditional means found",
          "during posterior-sample checks."
        )
      )
    }
    
    
    mu_ratio <- sampled_mu_mean /
      pred$.mu_mean
    
    
    median_mu_ratio <- stats::median(
      mu_ratio,
      na.rm = TRUE
    )
    
    
    if (
      !is.finite(median_mu_ratio) ||
      median_mu_ratio < 0.5 ||
      median_mu_ratio > 2
    ) {
      warning(
        paste(
          "Posterior-sample conditional means differ substantially",
          "from `summary.fitted.values` on the response scale.",
          "Check exposure, offset, or link handling."
        )
      )
    }
  }
  
  
  # 9. Return --------------------------------------------------------------
  
  out <- list(
    predictions = pred
  )
  
  
  if (keep_fit) {
    out$fit <- fit
  }
  
  
  out
}
