sprint2026_path <- here::here()

dengue_path <- fs::path(sprint2026_path, "dengue/state")
state_scores_path  <- fs::path(dengue_path, "outputs/cv_scores_state")
state_matrics_path  <- fs::path(dengue_path, "outputs/cv_metrics_state")

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

setup_hpc_library()

# 1. Collect scored state-week files --------------------------------------

state_sample_scores <- fs::dir_ls(
  state_scores_path,
  regexp = "\\.rds$",
  type = "file"
) |>
  purrr::map(readRDS) |>
  purrr::list_rbind()

# Basic checks
state_sample_scores |>
  dplyr::count(.split_id, .model_id)

state_sample_scores |>
  dplyr::group_by(.split_id, .model_id) |>
  dplyr::summarise(
    n_states = dplyr::n_distinct(uf_code),
    n_weeks = dplyr::n_distinct(.lead_week),
    n_rows = dplyr::n(),
    .groups = "drop"
  )

# 2. Challenge metric: average over full period by state and split --------

state_metrics_split <- state_sample_scores |>
  dplyr::group_by(
    .split_id, .model_id, .model_label, uf_code
  ) |>
  dplyr::summarise(
    wis = mean(wis_score, na.rm = TRUE),
    crps = mean(crps_sample, na.rm = TRUE),
    mae = mean(abs(cases - .mu_mean), na.rm = TRUE),
    rmse = sqrt(mean((cases - .mu_mean)^2, na.rm = TRUE)),
    bias = mean(.mu_mean - cases, na.rm = TRUE),
    coverage_95 = mean(coverage_95_flag, na.rm = TRUE),
    n_weeks = dplyr::n_distinct(.lead_week),
    .groups = "drop"
  )

# 3. Overall ranking across states and splits -----------------------------

model_ranking <- state_metrics_split |>
  dplyr::group_by(.model_id, .model_label) |>
  dplyr::summarise(
    wis_mean = mean(wis, na.rm = TRUE),
    wis_median = median(wis, na.rm = TRUE),
    wis_sd = sd(wis, na.rm = TRUE),
    wis_iqr = IQR(wis, na.rm = TRUE),
    crps_mean = mean(crps, na.rm = TRUE),
    mae_mean = mean(mae, na.rm = TRUE),
    rmse_mean = mean(rmse, na.rm = TRUE),
    bias_mean = mean(bias, na.rm = TRUE),
    coverage_95_mean = mean(coverage_95, na.rm = TRUE),
    n_state_splits = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(wis_mean) |>
  dplyr::mutate(rank_wis = dplyr::row_number())

model_ranking

# 4. Ranking by validation split -----------------------------------------

model_ranking_by_split <- state_metrics_split |>
  dplyr::group_by(.split_id, .model_id, .model_label) |>
  dplyr::summarise(
    wis_mean = mean(wis, na.rm = TRUE),
    wis_median = median(wis, na.rm = TRUE),
    crps_mean = mean(crps, na.rm = TRUE),
    mae_mean = mean(mae, na.rm = TRUE),
    coverage_95_mean = mean(coverage_95, na.rm = TRUE),
    n_states = dplyr::n_distinct(uf_code),
    .groups = "drop"
  ) |>
  dplyr::group_by(.split_id) |>
  dplyr::arrange(wis_mean, .by_group = TRUE) |>
  dplyr::mutate(rank_wis = dplyr::row_number()) |>
  dplyr::ungroup()

model_ranking_by_split

# 5. Ranking by state -----------------------------------------------------

model_ranking_by_state <- state_metrics_split |>
  dplyr::group_by(uf_code, .model_id, .model_label) |>
  dplyr::summarise(
    wis_mean = mean(wis, na.rm = TRUE),
    crps_mean = mean(crps, na.rm = TRUE),
    mae_mean = mean(mae, na.rm = TRUE),
    coverage_95_mean = mean(coverage_95, na.rm = TRUE),
    n_splits = dplyr::n_distinct(.split_id),
    .groups = "drop"
  ) |>
  dplyr::group_by(uf_code) |>
  dplyr::arrange(wis_mean, .by_group = TRUE) |>
  dplyr::mutate(rank_wis = dplyr::row_number()) |>
  dplyr::ungroup()

model_ranking_by_state

# 6. Optional: lead-week diagnostics --------------------------------------

model_metrics_lead_week <- state_sample_scores |>
  dplyr::group_by(
    .split_id, .model_id, .model_label, .lead_week
  ) |>
  dplyr::summarise(
    wis = mean(wis_score, na.rm = TRUE),
    crps = mean(crps_sample, na.rm = TRUE),
    mae = mean(abs(cases - .mu_mean), na.rm = TRUE),
    rmse = sqrt(mean((cases - .mu_mean)^2, na.rm = TRUE)),
    bias = mean(.mu_mean - cases, na.rm = TRUE),
    coverage_95 = mean(coverage_95_flag, na.rm = TRUE),
    .groups = "drop"
  )

# 7. Save summaries -------------------------------------------------------

saveRDS(state_metrics_split, fs::path(state_matrics_path, "state_metrics_split.rds"))
saveRDS(model_ranking, fs::path(state_matrics_path, "model_ranking.rds"))
saveRDS(model_ranking_by_split, fs::path(state_matrics_path, "model_ranking_by_split.rds"))
saveRDS(model_ranking_by_state, fs::path(state_matrics_path, "model_ranking_by_state.rds"))
saveRDS(model_metrics_lead_week, fs::path(state_matrics_path, "model_metrics_lead_week.rds"))



