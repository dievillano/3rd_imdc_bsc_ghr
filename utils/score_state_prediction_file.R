score_state_prediction_file <- function(
    file,
    out_dir,
    wis_quantiles = c(
      0.025, 0.05, 0.10, 0.25,
      0.50,
      0.75, 0.90, 0.95, 0.975
    )
) {
  
  pred <- readRDS(file)
  
  scores <- pred |>
    dplyr::mutate(
      .pred_quantiles = purrr::map(
        .pred_samples,
        \(samples) stats::quantile(
          samples,
          probs = wis_quantiles,
          na.rm = TRUE,
          names = FALSE
        )
      ),
      wis_score = purrr::map2_dbl(
        cases,
        .pred_quantiles,
        \(obs, pred_quantiles) scoringutils::wis(
          observed = obs,
          predicted = pred_quantiles,
          quantile_level = wis_quantiles
        )
      ),
      crps_sample = purrr::map2_dbl(
        cases,
        .pred_samples,
        \(obs, pred_sample) scoringutils::crps_sample(
          observed = obs,
          predicted = pred_sample
        )
      ),
      coverage_95_flag = purrr::map2_lgl(
        cases,
        .pred_quantiles,
        \(obs, pred_quantiles) {
          obs >= pred_quantiles[1] && obs <= pred_quantiles[9]
        }
      )
    ) |>
    dplyr::select(-.pred_samples, -.pred_quantiles)
  
  fs::dir_create(out_dir)
  
  out_file <- fs::path(out_dir, fs::path_file(file))
  saveRDS(scores, out_file)
  
  rm(pred, scores)
  gc()
  
  invisible(out_file)
}