fit_one_inla <- function(
    formula_string,
    data,
    family = "nbinomial",
    offset = NULL,
    nthreads_inla = 1,
    control_compute = list(),
    control_predictor = list(),
    formula_env = parent.frame(),
    ...
) {
  
  form <- stats::as.formula(formula_string, env = formula_env)
  
  default_control_compute <- list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = FALSE
  )
  
  control_compute <- utils::modifyList(
    default_control_compute,
    control_compute
  )
  
  if (!is.null(offset)) {
    if (!offset %in% names(data)) {
      stop("Offset/exposure column not found: ", offset)
    }
    
    data$E <- data[[offset]]
    
    if (any(is.na(data$E))) stop("E contains NA values.")
    if (any(!is.finite(data$E))) stop("E contains non-finite values.")
    if (any(data$E <= 0)) stop("E must be strictly positive.")
  }
  
  INLA::inla(
    formula = form,
    family = family,
    data = data,
    E = E,
    control.compute = control_compute,
    control.predictor = control_predictor,
    num.threads = nthreads_inla,
    ...
  )
}