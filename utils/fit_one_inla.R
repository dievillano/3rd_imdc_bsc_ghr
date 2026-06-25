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
  
  # Using INLA directly
  INLA::inla(
    formula = form,
    family = family,
    data = data,
    E = if (!is.null(offset)) data[[offset]] else NULL,
    control.compute = control_compute,
    control.predictor = control_predictor,
    num.threads = nthreads_inla,
    ...
  )
}