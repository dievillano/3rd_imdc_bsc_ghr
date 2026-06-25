sprint2026_path <- here::here()
data_path <- fs::path(sprint2026_path, "data")

processed_data_path <- fs::path(data_path, "processed/health_region")
processed_shp_path <- fs::path(processed_data_path, "shp")
processed_graph_path <- fs::path(processed_data_path, "graph")
internal_splits_path <- fs::path(processed_data_path, "internal_splits")

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

ggplot2::set_theme(ggplot2::theme_bw())

# 1. Read internal split --------------------------------------------------

internal_splits <- readRDS(
  fs::path(internal_splits_path, "dengue_hr_internal_splits.rds")
)

# 2. Scale splits ---------------------------------------------------------

climate_predictors <- c(
  "temp_med_weight_rollmean_12_lag_8",
  "precip_min_weight_rollmean_12_lag_4",
  "precip_min_weight_anom_rollmean_12_lag_8",
  "rel_humid_max_weight_rollmean_8_lag_4",
  "rel_humid_min_weight_rollmean_24_lag_8",
  "rainy_days_weight_lag_8",
  "enso_lag_8"
)

predictors <- c(
  climate_predictors,
  "koppen"
)

scaled_val_splits <- purrr::map(
  internal_splits,
  \(split) {
    
    scaled <- scale_split(
      train_data = split$train, 
      validation_data = split$validation,
      predictors = tidyselect::all_of(climate_predictors),
      overwrite = TRUE
    )
    
    list(
      train = make_inla_vars(scaled$train, climate_predictors),
      validation = make_inla_vars(scaled$validation, climate_predictors),
      scaler = scaled$scaler,
      metadata = split$metadata
    )
  }
)

# 3. Formula environment --------------------------------------------------

hr_graph <- INLA::inla.read.graph(
  fs::path(processed_graph_path, "hr_graph.graph")
)

prec_prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))

bym2_prior <- list(
  prec = list(prior = "pc.prec", param = c(0.5, 0.01)),
  phi  = list(prior = "pc", param = c(0.5, 2/3))
)

formula_env <- rlang::env(
  graph = hr_graph,
  bym2_prior = bym2_prior,
  prec_prior = prec_prior
)

# 3. Model specification --------------------------------------------------

climate_lin <- climate_predictors

climate_nl <- paste0(
  "f(",
  climate_predictors,
  "_q10, model = 'rw2', scale.model = TRUE, constr = TRUE, hyper = prec_prior)"
)

climate_slopes_koppen <- paste0(
  climate_predictors,
  " + f(koppen_id_",
  climate_predictors,
  ", ",
  climate_predictors,
  ", model = 'iid', constr = TRUE, hyper = prec_prior)"
)

re_baseline <- c(
  "f(hr_id, model = 'bym2', graph = hr_graph, scale.model = TRUE, hyper = bym2_prior)",
  "f(week_id, model = 'rw2', cyclic = TRUE, scale.model = TRUE, constr = TRUE, hyper = prec_prior)"
)

re_state_season <- c(
  "f(hr_id, model = 'bym2', graph = hr_graph, scale.model = TRUE, hyper = bym2_prior)",
  "f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE, scale.model = TRUE, constr = TRUE, hyper = prec_prior)"
)

formulas <- list(
  m0_baseline = make_formula(
    re_baseline
  ),
  
  m1_koppen = make_formula(
    c("koppen", re_baseline)
  ),
  
  m2_linear_climate = make_formula(
    c("koppen", climate_lin, re_baseline)
  ),
  
  m3_nonlinear_climate = make_formula(
    c("koppen", climate_nl, re_baseline)
  ),
  
  m4_koppen_slopes = make_formula(
    c("koppen", climate_slopes_koppen, re_baseline)
  ),
  
  m5_state_season = make_formula(
    re_state_season
  ),
  
  m6_state_season_koppen = make_formula(
    c("koppen", re_state_season)
  ),
  
  m7_state_season_linear_climate = make_formula(
    c("koppen", climate_lin, re_state_season)
  ),
  
  m8_state_season_nonlinear_climate = make_formula(
    c("koppen", climate_nl, re_state_season)
  ),
  
  m9_state_season_koppen_slopes = make_formula(
    c("koppen", climate_slopes_koppen, re_state_season)
  )
)

cv_model_specs <- tibble::tibble(
  model_id = names(formulas),
  model_label = c(
    "Baseline: spatial + seasonal",
    "Baseline + Koppen",
    "Baseline + linear climate",
    "Baseline + nonlinear climate",
    "Baseline + climate slopes by Koppen",
    "State-season baseline",
    "State-season + Koppen",
    "State-season + linear climate",
    "State-season + nonlinear climate",
    "State-season + climate slopes by Koppen"
  ),
  formula_string = purrr::map_chr(formulas, \(x) {
    paste(deparse(x), collapse = " ")
  }),
  predictors = list(
    character(0),
    "koppen",
    c("koppen", climate_predictors),
    c("koppen", paste0(climate_predictors, "_q10")),
    c(
      "koppen",
      climate_predictors,
      paste0("koppen_id_", climate_predictors)
    ),
    character(0),
    "koppen",
    c("koppen", climate_predictors),
    c("koppen", paste0(climate_predictors, "_q10")),
    c(
      "koppen",
      climate_predictors,
      paste0("koppen_id_", climate_predictors)
    )
  ),
  effect_type = c(
    "baseline",
    "koppen_main",
    "linear_climate",
    "nonlinear_climate",
    "koppen_random_slopes",
    "state_season_baseline",
    "state_season_koppen",
    "state_season_linear_climate",
    "state_season_nonlinear_climate",
    "state_season_koppen_random_slopes"
  ),
  family = "nbinomial",
  offset = "pop100k"
)

# 4. Run CV ---------------------------------------------------------------

# Try first version first

cv_model_specs_baseline <- cv_model_specs |> 
  dplyr::slice_head(n = 1)

cv_results <- run_cv_inla(
  model_specs = cv_model_specs_baseline,
  splits = scaled_val_splits,
  outcome = "cases",
  family = "nbinomial",
  offset = "pop100k",
  nthreads_inla = 8,
  forecast_ppd = TRUE,
  control_compute = list(config = TRUE),
  control_predictor = list(
    compute = TRUE,
    link = 1
  ),
  n_samples = 1000,
  formula_env = formula_env,
  keep_fit = FALSE,
  seed = 2016,
  verbose = TRUE
)

# 5. Collect predictions --------------------------------------------------

cv_predictions <- collect_cv_predictions(cv_results)

# Checks

cv_predictions |>
  dplyr::count(.split_id, .model_id, .lead_week)

cv_predictions |>
  dplyr::group_by(.split_id) |>
  dplyr::summarise(
    n_weeks = dplyr::n_distinct(.lead_week),
    n_regions = dplyr::n_distinct(regional_geocode),
    n = dplyr::n()
  )

cv_predictions_state <- cv_predictions |>
  dplyr::group_by(
    .split_id, .model_id, .model_label, uf_code, date, epiweek, .lead_week
  ) |>
  dplyr::summarise(
    cases = sum(cases, na.rm = TRUE),
    .mu_mean = sum(.mu_mean, na.rm = TRUE),
    .pred_samples = list(
      purrr::reduce(.pred_samples, `+`)
    ),
    .groups = "drop"
  )

# 6. Scoring --------------------------------------------------------------

wis_quantiles <- c(
  0.025, 0.05, 0.10, 0.25,
  0.50,
  0.75, 0.90, 0.95, 0.975
)

state_sample_scores <- cv_predictions_state |>
  dplyr::mutate(
    .pred_quantiles = purrr::map(
      .pred_samples,
      \(samples) {
        stats::quantile(
          samples,
          probs = wis_quantiles,
          na.rm = TRUE,
          names = FALSE
        )
      }
    ),
    wis_score = purrr::map2_dbl(
      cases,
      .pred_quantiles,
      \(obs, pred_quantiles) {
        scoringutils::wis(
          observed = obs,
          predicted = pred_quantiles,
          quantile_level = wis_quantiles
        )
      }
    ),
    crps_sample = purrr::map2_dbl(
      cases,
      .pred_samples,
      \(obs, pred_sample) {
        scoringutils::crps_sample(
          observed = obs,
          predicted = pred_sample
        )
      }
    ),
    coverage_95_flag = purrr::map2_lgl(
      cases,
      .pred_quantiles,
      \(obs, pred_quantiles) {
        obs >= pred_quantiles[1] && obs <= pred_quantiles[9]
      }
    )
  )

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
    n_weeks = dplyr::n_distinct(date),
    .groups = "drop"
  ) |>
  dplyr::arrange(.split_id, wis)

state_metrics_overall <- state_metrics_split |>
  dplyr::group_by(.model_id, .model_label) |>
  dplyr::summarise(
    dplyr::across(
      wis:coverage_95,
      list(
        mean = \(x) mean(x, na.rm = TRUE),
        sd = \(x) sd(x, na.rm = TRUE),
        median = \(x) median(x, na.rm = TRUE),
        iqr = \(x) IQR(x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(wis_mean)

state_metrics_lead_week <- state_sample_scores |>
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



