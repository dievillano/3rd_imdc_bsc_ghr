make_formula <- function(terms) {
  stats::as.formula(
    paste(
      "cases ~ 1 +",
      paste(terms, collapse = " + ")
    )
  )
}