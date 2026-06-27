sprint2026_path <- here::here()
data_path <- fs::path(sprint2026_path, "data")

processed_data_path <- fs::path(data_path, "processed/health_region")
internal_splits_path <- fs::path(processed_data_path, "internal_splits")

dengue_path <- fs::path(sprint2026_path, "dengue/state")
screening_path <- fs::path(dengue_path, "outputs/screening")

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

ggplot2::set_theme(ggplot2::theme_bw())

# 1. Read internal split --------------------------------------------------

internal_splits <- readRDS(
  fs::path(internal_splits_path, "dengue_hr_internal_splits.rds")
)

train <- internal_splits$`1`$train

rm(internal_splits)
gc()

train <- train |> 
  dplyr::mutate(
    inc100k = 100000 * cases / population,
    inc100k_log = log1p(inc100k),
    week_id = factor(week_id),
    year_id = factor(year_id),
    hr_id = factor(hr_id)
  )

lagged_vars <- grep(
  "_lag_[0-9]+$",
  names(train),
  value = TRUE
)

fixed_vars <- c(
  "hr_id",
  "week_id",
  "year_id",
  "biome",
  "koppen"
)

predictor_vars <- c(
  lagged_vars,
  fixed_vars
)

glmnet_data <- train |>
  dplyr::select(
    inc100k_log,
    dplyr::all_of(predictor_vars)
  ) |>
  tidyr::drop_na()

x <- Matrix::sparse.model.matrix(
  inc100k_log ~ . - 1,
  data = glmnet_data
)

y <- glmnet_data$inc100k_log

fit_glmnet <- glmnet::glmnet(
  x = x,
  y = y,
  family = "gaussian",
  alpha = 0.5,
  standardize = TRUE
)

lambda_summary <- purrr::map_dfr(fit_glmnet$lambda, \(lam) {
  coefs <- coef(fit_glmnet, s = lam)
  
  tibble::tibble(
    lambda = lam,
    n_selected = sum(as.numeric(coefs) != 0) - 1
  )
})

lambda_summary |>
  dplyr::arrange(n_selected)

target_n <- 30

chosen_lambda <- lambda_summary |>
  dplyr::filter(n_selected <= target_n) |>
  dplyr::arrange(dplyr::desc(n_selected)) |>
  dplyr::slice(1) |>
  dplyr::pull(lambda)

coef_chosen <- coef(fit_glmnet, s = chosen_lambda)

selected_predictors <- tibble::tibble(
  predictor = rownames(coef_chosen),
  coefficient = as.numeric(coef_chosen)
) |>
  dplyr::filter(
    coefficient != 0,
    predictor != "(Intercept)",
    !grepl("^(hr_id|week_id|year_id)", predictor)
  ) |>
  dplyr::arrange(dplyr::desc(abs(coefficient)))

selected_predictors <- selected_predictors |>
  dplyr::mutate(
    family = dplyr::case_when(
      grepl("^temp", predictor) ~ "Temperature",
      grepl("^precip", predictor) ~ "Precipitation",
      grepl("^rel_humid", predictor) ~ "Humidity",
      grepl("^(enso|pdo|iod)", predictor) ~ "Ocean",
      grepl("^biome", predictor) ~ "Biome",
      grepl("^koppen", predictor) ~ "Koppen",
      TRUE ~ "Other"
    )
  )

saveRDS(
  selected_predictors,
  fs::path(
    screening_path,
    "glmnet_selected_predictors.rds"
  )
)

selected_predictors |> 
  dplyr::count(family, sort = TRUE)

selected_predictors |> 
  dplyr::filter(family == "Temperature") 

temp_selected <- selected_predictors |> 
  dplyr::filter(family == "Temperature") |> 
  dplyr::arrange(predictor) |> 
  dplyr::pull(predictor)

GHRexplore::plot_correlation(
  data = train,
  var = temp_selected,
  method = "pearson",
  plot_type = c("raster", "number")
)

selected_predictors |> 
  dplyr::filter(family == "Precipitation") 

precip_selected <- selected_predictors |> 
  dplyr::filter(family == "Precipitation") |> 
  dplyr::arrange(predictor) |> 
  dplyr::pull(predictor)

GHRexplore::plot_correlation(
  data = train,
  var = precip_selected,
  method = "pearson",
  plot_type = c("raster", "number")
)

selected_predictors |> 
  dplyr::filter(family == "Humidity") 

rel_humid_selected <- selected_predictors |> 
  dplyr::filter(family == "Humidity") |> 
  dplyr::arrange(predictor) |> 
  dplyr::pull(predictor)

GHRexplore::plot_correlation(
  data = train,
  var = rel_humid_selected,
  method = "pearson",
  plot_type = c("raster", "number")
)

selected_predictors |> 
  dplyr::filter(family == "Ocean") 

ocean_selected <- selected_predictors |> 
  dplyr::filter(family == "Ocean") |> 
  dplyr::arrange(predictor) |> 
  dplyr::pull(predictor)

GHRexplore::plot_correlation(
  data = train,
  var = ocean_selected,
  method = "pearson",
  plot_type = c("raster", "number")
)

selected_predictors |> 
  dplyr::filter(family == "Koppen") 

selected_predictors |> 
  dplyr::filter(family == "Other") 

screened_predictors_num <- c(
  "temp_med_weight_rollmean_12_lag_8",
  "precip_min_weight_rollmean_12_lag_4",
  "precip_min_weight_anom_rollmean_12_lag_8",
  "rel_humid_max_weight_rollmean_8_lag_4",
  "rel_humid_min_weight_rollmean_24_lag_8",
  "enso_lag_8",
  "rainy_days_weight_lag_8"
)

GHRexplore::plot_correlation(
  data = train,
  var = screened_predictors_num,
  method = "pearson",
  plot_type = c("raster", "number")
)

screened_predictors <- c(
  screened_predictors_num,
  "koppen"
)
