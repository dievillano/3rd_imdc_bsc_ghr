args <- commandArgs(trailingOnly = TRUE)

input_file <- args[[1]]

sprint2026_path <- here::here()

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

setup_hpc_library()

library(scoringutils)

dengue_path <- fs::path(sprint2026_path, "dengue/state")
state_scores_path  <- fs::path(dengue_path, "outputs/cv_scores_state")

wis_quantiles <- c(
  0.025, 0.05, 0.10, 0.25,
  0.50,
  0.75, 0.90, 0.95, 0.975
)

cat("Scoring file: ", input_file, "\n", sep = "")

pred <- readRDS(input_file)

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

out_file <- fs::path(
  state_scores_path,
  fs::path_file(input_file)
)

saveRDS(scores, out_file)

cat("Saved: ", out_file, "\n", sep = "")