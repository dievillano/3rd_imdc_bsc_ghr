collect_cv_predictions <- function(cv_results) {
  purrr::imap_dfr(
    cv_results, \(split_results, split_id) {
      purrr::imap_dfr(
        split_results, \(model_result, model_id) {
          model_result$predictions
        }
      )
    }
  )
}