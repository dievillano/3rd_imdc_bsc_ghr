
sprint2026_path <- here::here()

dengue_path <- fs::path(sprint2026_path, "dengue/state")

model_specs_path <- fs::path(dengue_path, "outputs/model_specs")
fs::dir_create(model_specs_path)

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

#-------------

climate_predictors <- c(
  "temp_med_weight_rollmean_12_lag_8",
  "precip_min_weight_rollmean_12_lag_4",
  "precip_min_weight_anom_rollmean_12_lag_8",
  "rel_humid_max_weight_rollmean_8_lag_4",
  "rel_humid_min_weight_rollmean_24_lag_8",
  "rainy_days_weight_lag_8",
  "enso_lag_8"
)

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

model_specs <- tibble::tibble(
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

saveRDS(model_specs, fs::path(model_specs_path, "cv_model_specs.rds"))

