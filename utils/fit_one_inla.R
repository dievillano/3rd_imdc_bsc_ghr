fit_one_inla <- function(
    formula_string,
    data,
    family = "nbinomial",
    exposure = NULL,
    offset = NULL,
    nthreads_inla = 1L,
    control_compute = list(),
    control_predictor = list(),
    formula_env = parent.frame(),
    ...
) {
  
  # 1. Validate formula ----------------------------------------------------
  
  if (
    !is.character(formula_string) ||
    length(formula_string) != 1L ||
    is.na(formula_string)
  ) {
    rlang::abort(
      "`formula_string` must be one non-missing character string."
    )
  }
  
  
  # 2. Validate data -------------------------------------------------------
  
  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data frame."
    )
  }
  
  
  # 3. Validate family -----------------------------------------------------
  
  if (
    !is.character(family) ||
    length(family) != 1L ||
    is.na(family)
  ) {
    rlang::abort(
      "`family` must be one non-missing character string."
    )
  }
  
  
  # 4. Validate threads ----------------------------------------------------
  
  if (
    length(nthreads_inla) != 1L ||
    !is.numeric(nthreads_inla) ||
    is.na(nthreads_inla) ||
    nthreads_inla < 1L ||
    nthreads_inla != as.integer(nthreads_inla)
  ) {
    rlang::abort(
      "`nthreads_inla` must be one positive integer."
    )
  }
  
  nthreads_inla <- as.integer(
    nthreads_inla
  )
  
  
  # 5. Validate controls ---------------------------------------------------
  
  if (!is.list(control_compute)) {
    rlang::abort(
      "`control_compute` must be a list."
    )
  }
  
  if (!is.list(control_predictor)) {
    rlang::abort(
      "`control_predictor` must be a list."
    )
  }
  
  
  # 6. Validate exposure ---------------------------------------------------
  #
  # `exposure` corresponds to INLA's E argument.
  #
  # For a log-link count model:
  #
  #   mu = E * exp(eta)
  #
  # The exposure is NOT part of the linear predictor.
  
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
    
    if (!exposure %in% names(data)) {
      rlang::abort(
        paste0(
          "Exposure column `",
          exposure,
          "` was not found in `data`."
        )
      )
    }
    
    exposure_values <- data[[exposure]]
    
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
    
    exposure_values <- NULL
  }
  
  
  # 7. Validate offset -----------------------------------------------------
  #
  # `offset` corresponds to INLA's offset argument and therefore lives
  # on the linear-predictor scale.
  #
  # Unlike an exposure, an offset may be negative, zero or positive.
  
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
    
    if (!offset %in% names(data)) {
      rlang::abort(
        paste0(
          "Offset column `",
          offset,
          "` was not found in `data`."
        )
      )
    }
    
    offset_values <- data[[offset]]
    
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
    
  } else {
    
    offset_values <- NULL
  }
  
  
  # Prevent accidental double use of the same column.
  
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
  
  
  # 8. Construct formula ---------------------------------------------------
  
  formula <- stats::as.formula(
    formula_string,
    env = formula_env
  )
  
  
  # 9. Additional INLA arguments ------------------------------------------
  
  dots <- list(...)
  
  reserved_arguments <- c(
    "formula",
    "family",
    "data",
    "E",
    "offset",
    "num.threads",
    "control.compute",
    "control.predictor"
  )
  
  duplicated_arguments <- intersect(
    names(dots),
    reserved_arguments
  )
  
  if (length(duplicated_arguments) > 0L) {
    rlang::abort(
      paste(
        "The following arguments must be supplied through",
        "`fit_one_inla()` rather than `...`:",
        paste(
          duplicated_arguments,
          collapse = ", "
        )
      )
    )
  }
  
  
  # 10. Build INLA call ----------------------------------------------------
  #
  # Build the argument list explicitly rather than using expressions such
  # as E = data[[exposure]]. This avoids the NSE issues encountered with
  # INLA while still allowing E and offset to be omitted completely when
  # they are NULL.
  
  inla_args <- list(
    formula = formula,
    family = family,
    data = data,
    num.threads = nthreads_inla,
    control.compute = control_compute,
    control.predictor = control_predictor
  )
  
  
  if (!is.null(exposure_values)) {
    inla_args$E <- exposure_values
  }
  
  
  if (!is.null(offset_values)) {
    inla_args$offset <- offset_values
  }
  
  
  if (length(dots) > 0L) {
    inla_args <- c(
      inla_args,
      dots
    )
  }
  
  
  # 11. Fit ---------------------------------------------------------------
  
  fit <- do.call(
    INLA::inla,
    inla_args
  )
  
  
  # 12. Return -------------------------------------------------------------
  
  fit
}
