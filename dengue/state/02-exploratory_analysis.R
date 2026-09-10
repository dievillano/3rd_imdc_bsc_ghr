sprint2026_path <- here::here()
data_path <- fs::path(sprint2026_path, "data")

processed_data_path <- fs::path(data_path, "processed/health_region")
processed_shp_path <- fs::path(processed_data_path, "shp")
internal_splits_path <- fs::path(processed_data_path, "internal_splits")

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

ggplot2::set_theme(ggplot2::theme_bw())

# 1. Read internal split --------------------------------------------------

internal_splits <- readRDS(
  fs::path(internal_splits_path, "dengue_hr_internal_splits.rds")
)

train <- internal_splits$`1`$train

# 2. Cases ----------------------------------------------------------------

# 2.1 National ------------------------------------------------------------

cases_date_national <- train |> 
  dplyr::group_by(date) |> 
  dplyr::summarise(
    cases = sum(cases),
    population = sum(population),
    .groups = "drop"
  ) |> 
  dplyr::mutate(
    inc100k = 100000 * cases / population
  )
  
summary(cases_date_national$cases)
hist(cases_date_national$cases)

summary(cases_date_national$inc100k)
hist(cases_date_national$inc100k)

ew41_dates <- train |>
  dplyr::filter(epiweek_num == 41) |>
  dplyr::mutate(
    month = lubridate::month(date, label = TRUE, abbr = FALSE),
    day = lubridate::day(date)
  ) |>
  dplyr::distinct(epiweek, date, month, day) |>
  dplyr::arrange(date)

GHRexplore::plot_timeseries(
  data = cases_date_national,
  var = "cases",
  type = "counts",
  time = "date"
) +
  ggplot2::geom_vline(
    data = ew41_dates,
    ggplot2::aes(
      xintercept = date
    ),
    colour = "red",
    linewidth = 0.2
  )

GHRexplore::plot_timeseries(
  data = cases_date_national,
  var = "cases",
  type = "inc",
  pop = "population",
  time = "date"
) +
  ggplot2::geom_vline(
    data = ew41_dates,
    ggplot2::aes(
      xintercept = date
    ),
    colour = "red",
    linewidth = 0.2
  )
  
cases_week_national <- train |>
  dplyr::group_by(week_id, date) |>
  dplyr::summarise(
    cases = sum(cases),
    population = sum(population),
    .groups = "drop"
  ) |> 
  dplyr::mutate(
    inc100k = 100000 * cases / population
  ) |> 
  dplyr::group_by(week_id) |> 
  dplyr::summarise(
    inc100k_median = median(inc100k),
    inc100k_q25 = quantile(inc100k, 0.25),
    inc100k_q75 = quantile(inc100k, 0.75),
    .groups = "drop"
  )

cases_week_national |> 
  ggplot2::ggplot(
    ggplot2::aes(week_id, inc100k_median)
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = inc100k_q25, ymax = inc100k_q75),
    fill = "lightgray", alpha = 0.8
  ) +
  ggplot2::geom_line(colour = "steelblue") 

cases_epiyear_week_national <- train |>
  dplyr::group_by(epiyear, week_id, date) |>
  dplyr::summarise(
    cases = sum(cases),
    population = sum(population),
    .groups = "drop"
  ) |> 
  dplyr::mutate(
    inc100k = 100000 * cases / population
  ) 

cases_epiyear_week_national |> 
  ggplot2::ggplot(
    ggplot2::aes(
      week_id, inc100k, colour = factor(epiyear), group = factor(epiyear)
    )
  ) +
  ggplot2::geom_line() 

peak_weeks_national <- train |>
  dplyr::group_by(epiyear, week_id) |>
  dplyr::summarise(
    cases = sum(cases),
    .groups = "drop"
  ) |> 
  dplyr::group_by(epiyear) |> 
  dplyr::slice_max(cases, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

summary(peak_weeks_national$week_id)
hist(peak_weeks_national$week_id)

forecast::ggAcf(
  log1p(cases_date_national$inc100k),
  lag.max = 104,
)

forecast::ggPacf(
  log1p(cases_date_national$inc100k),
  lag.max = 104,
)
  
# 2.2 States --------------------------------------------------------------

cases_date_state <- train |> 
  dplyr::group_by(uf_code, date) |> 
  dplyr::summarise(
    cases = sum(cases),
    population = sum(population),
    .groups = "drop"
  ) |> 
  dplyr::mutate(
    inc100k = 100000 * cases / population,
    inc100k_log = log1p(inc100k)
  )

summary(cases_date_state$cases)
hist(cases_date_state$cases, breaks = 50)

summary(cases_date_state$inc100k)
hist(cases_date_state$inc100k, breaks = 50)

cases_date_state |> 
  ggplot2::ggplot(
    ggplot2::aes(cases)
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(factor(uf_code)), scales = "free_y"
  ) +
  ggplot2::geom_histogram(bins = 50) 

cases_date_state |> 
  ggplot2::ggplot(
    ggplot2::aes(inc100k)
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(factor(uf_code)), scales = "free_y"
  ) +
  ggplot2::geom_histogram(bins = 50) 

cases_date_state |> 
  ggplot2::ggplot(
    ggplot2::aes(factor(uf_code), cases)
  ) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip()

cases_date_state |> 
  ggplot2::ggplot(
    ggplot2::aes(factor(uf_code), inc100k)
  ) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip()

GHRexplore::plot_timeseries(
  data = cases_date_state,
  var = "cases",
  type = "counts",
  time = "date",
  area = "uf_code",
  facet = TRUE,
  free_y_scale = TRUE
) +
  ggplot2::geom_vline(
    data = ew41_dates,
    ggplot2::aes(
      xintercept = date
    ),
    colour = "red",
    linewidth = 0.2
  )

GHRexplore::plot_timeseries(
  data = cases_date_state,
  var = "cases",
  type = "inc",
  pop = "population",
  time = "date",
  area = "uf_code",
  facet = TRUE,
  free_y_scale = TRUE
) +
  ggplot2::geom_vline(
    data = ew41_dates,
    ggplot2::aes(
      xintercept = date
    ),
    colour = "red",
    linewidth = 0.2
  )

cases_week_state <- train |>
  dplyr::group_by(uf_code, week_id, date) |>
  dplyr::summarise(
    cases = sum(cases),
    population = sum(population),
    .groups = "drop"
  ) |> 
  dplyr::mutate(
    inc100k = 100000 * cases / population
  ) |> 
  dplyr::group_by(uf_code, week_id) |> 
  dplyr::summarise(
    inc100k_median = median(inc100k),
    inc100k_q25 = quantile(inc100k, 0.25),
    inc100k_q75 = quantile(inc100k, 0.75),
    .groups = "drop"
  )

cases_week_state |> 
  ggplot2::ggplot(
    ggplot2::aes(week_id, inc100k_median)
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = inc100k_q25, ymax = inc100k_q75),
    fill = "lightgray", alpha = 0.8
  ) +
  ggplot2::geom_line(colour = "steelblue") +
  ggplot2::facet_wrap(
    ggplot2::vars(factor(uf_code)), scales = "free_y"
  )

cases_epiyear_week_state <- train |>
  dplyr::group_by(uf_code, epiyear, week_id, date) |>
  dplyr::summarise(
    cases = sum(cases),
    population = sum(population),
    .groups = "drop"
  ) |> 
  dplyr::mutate(
    inc100k = 100000 * cases / population
  ) 

cases_epiyear_week_state |> 
  ggplot2::ggplot(
    ggplot2::aes(
      week_id, inc100k, colour = factor(epiyear), group = factor(epiyear)
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(
    ggplot2::vars(factor(uf_code)), scales = "free_y"
  )

peak_weeks_state <- train |>
  dplyr::group_by(uf_code, epiyear, week_id) |>
  dplyr::summarise(
    cases = sum(cases),
    .groups = "drop"
  ) |> 
  dplyr::group_by(uf_code, epiyear) |> 
  dplyr::slice_max(cases, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

summary(peak_weeks_state$week_id)
hist(peak_weeks_state$week_id)

# 2.3 Health regions ------------------------------------------------------

train <- train |> 
  dplyr::mutate(
    inc100k = 100000 * (cases / population),
    inc100k_log = log1p(inc100k)
  )

summary(train$cases)
hist(train$cases, breaks = 50)

summary(train$inc100k)
hist(train$inc100k, breaks = 50)

cases_date_hr <- train |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    inc100k_median = median(inc100k),
    inc100k_q25 = quantile(inc100k, 0.25),
    inc100k_q75 = quantile(inc100k, 0.75),
    .groups = "drop"
  ) 

cases_date_hr |> 
  ggplot2::ggplot(ggplot2::aes(date, inc100k_median)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = inc100k_q25, ymax = inc100k_q75),
    fill = "lightgray", alpha = 0.8
  ) +
  ggplot2::geom_line(colour = "steelblue") +
  ggplot2::geom_vline(
    data = ew41_dates,
    ggplot2::aes(
      xintercept = date
    ),
    colour = "red",
    linewidth = 0.2
  ) +
  ggplot2::scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  )

cases_week_hr <- train |>
  dplyr::group_by(week_id) |>
  dplyr::summarise(
    inc100k_median = median(inc100k),
    inc100k_q25 = quantile(inc100k, 0.25),
    inc100k_q75 = quantile(inc100k, 0.75),
    .groups = "drop"
  ) 

cases_week_hr |> 
  ggplot2::ggplot(
    ggplot2::aes(week_id, inc100k_median)
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = inc100k_q25, ymax = inc100k_q75),
    fill = "lightgray", alpha = 0.8
  ) +
  ggplot2::geom_line(colour = "steelblue") 

cases_epiyear_week_hr <- train |>
  dplyr::group_by(epiyear, week_id) |>
  dplyr::summarise(
    inc100k_median = median(inc100k),
    inc100k_q25 = quantile(inc100k, 0.25),
    inc100k_q75 = quantile(inc100k, 0.75),
    .groups = "drop"
  ) 

cases_epiyear_week_hr |> 
  ggplot2::ggplot(
    ggplot2::aes(
      week_id, inc100k_median, colour = factor(epiyear), 
      group = factor(epiyear)
    )
  ) +
  ggplot2::geom_line() 

peak_weeks_hr <- train |>
  dplyr::group_by(regional_geocode, epiyear) |>
  dplyr::slice_max(cases, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

summary(peak_weeks_hr$week_id)
hist(peak_weeks_hr$week_id)

# Spatial distribution
hr_shp <- readRDS(fs::path(processed_shp_path, "hr_shapefile.rds"))

GHRexplore::plot_map(
  train, 
  var = "cases", 
  time = "date",
  type = "counts", 
  area = "regional_geocode",
  map = hr_shp,
  map_area = "regional_geocode",
  aggregate_time = "all",
  bins = 5,
  bins_method = "quantile"
)

GHRexplore::plot_map(
  train, 
  var = "cases", 
  time = "date",
  type = "counts", 
  area = "regional_geocode",
  map = hr_shp,
  map_area = "regional_geocode",
  aggregate_time = "year",
  bins = 5,
  bins_method = "quantile"
)

GHRexplore::plot_map(
  train, 
  var = "cases", 
  time = "date",
  type = "inc", 
  pop = "population",
  area = "regional_geocode",
  map = hr_shp,
  map_area = "regional_geocode",
  aggregate_time = "all",
  bins = 5,
  bins_method = "quantile"
)

GHRexplore::plot_map(
  train, 
  var = "cases", 
  time = "date",
  type = "inc", 
  pop = "population",
  area = "regional_geocode",
  map = hr_shp,
  map_area = "regional_geocode",
  aggregate_time = "year",
  bins = 5,
  bins_method = "quantile"
)

# Proportion of zeros
mean(train$cases == 0)

zero_hr_summary <- train |>
  dplyr::group_by(regional_geocode) |>
  dplyr::summarise(
    prop_zero = mean(cases == 0),
    inc100k_median = median(inc100k),
    .groups = "drop"
  ) |> 
  dplyr::arrange(dplyr::desc(prop_zero))

summary(zero_hr_summary$prop_zero)
hist(zero_hr_summary$prop_zero)

zero_hr_summary |> 
  ggplot2::ggplot(ggplot2::aes(prop_zero, log1p(inc100k_median))) +
  ggplot2::geom_point(colour = "lightgray") +
  ggplot2::geom_smooth()

# 3. Predictors -----------------------------------------------------------

clim_raw_predictors <- colnames(train)[
  grepl("^(temp|precip|rel_humid).*_(weight|anom)$", colnames(train))
]

train |> 
  ggplot2::ggplot(ggplot2::aes(temp_min_weight)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(temp_med_weight)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(temp_max_weight)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(precip_min_weight)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(log1p(precip_min_weight))) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(sqrt(precip_min_weight))) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(precip_med_weight)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(log1p(precip_med_weight))) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(sqrt(precip_med_weight))) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(precip_max_weight)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(log1p(precip_max_weight))) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(sqrt(precip_max_weight))) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(rel_humid_min_weight)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(rel_humid_med_weight)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(rel_humid_max_weight)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(temp_min_weight_anom)) +
  ggplot2::geom_histogram(bins = 50)

summary(train$temp_min_weight_anom)

train |> 
  ggplot2::ggplot(ggplot2::aes(temp_med_weight_anom)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(temp_max_weight_anom)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(precip_min_weight_anom)) +
  ggplot2::geom_histogram(bins = 50)

summary(train$precip_min_weight_anom)

train |> 
  ggplot2::ggplot(ggplot2::aes(precip_med_weight_anom)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(precip_max_weight_anom)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(rel_humid_min_weight_anom)) +
  ggplot2::geom_histogram(bins = 50)

summary(train$rel_humid_min_weight_anom)

train |> 
  ggplot2::ggplot(ggplot2::aes(rel_humid_med_weight_anom)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(ggplot2::aes(rel_humid_max_weight_anom)) +
  ggplot2::geom_histogram(bins = 50)

train |> 
  ggplot2::ggplot(
    ggplot2::aes(inc100k_log, biome)
  ) +
  ggplot2::geom_boxplot() 

train |> 
  ggplot2::ggplot(
    ggplot2::aes(inc100k_log, koppen)
  ) +
  ggplot2::geom_boxplot() 

train |> 
  ggplot2::ggplot(
    ggplot2::aes(temp_med_weight, biome)
  ) +
  ggplot2::geom_boxplot() 

train |> 
  ggplot2::ggplot(
    ggplot2::aes(temp_med_weight, koppen)
  ) +
  ggplot2::geom_boxplot() 

train |> 
  ggplot2::ggplot(
    ggplot2::aes(precip_med_weight, biome)
  ) +
  ggplot2::geom_boxplot() 

train |> 
  ggplot2::ggplot(
    ggplot2::aes(precip_med_weight, koppen)
  ) +
  ggplot2::geom_boxplot() 

train |> 
  ggplot2::ggplot(
    ggplot2::aes(rel_humid_med_weight, biome)
  ) +
  ggplot2::geom_boxplot() 

train |> 
  ggplot2::ggplot(
    ggplot2::aes(rel_humid_med_weight, koppen)
  ) +
  ggplot2::geom_boxplot() 

cases_week_hr_biome <- train |>
  dplyr::group_by(biome, week_id) |>
  dplyr::summarise(
    inc100k_median = median(inc100k),
    inc100k_q25 = quantile(inc100k, 0.25),
    inc100k_q75 = quantile(inc100k, 0.75),
    .groups = "drop"
  ) 

cases_week_hr_biome |> 
  ggplot2::ggplot(
    ggplot2::aes(week_id, inc100k_median)
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = inc100k_q25, ymax = inc100k_q75),
    fill = "lightgray", alpha = 0.8
  ) +
  ggplot2::geom_line(colour = "steelblue") +
  ggplot2::facet_wrap(
    ggplot2::vars(biome)
  )

cases_week_hr_koppen <- train |>
  dplyr::group_by(koppen, week_id) |>
  dplyr::summarise(
    inc100k_median = median(inc100k),
    inc100k_q25 = quantile(inc100k, 0.25),
    inc100k_q75 = quantile(inc100k, 0.75),
    .groups = "drop"
  ) 

cases_week_hr_koppen |> 
  ggplot2::ggplot(
    ggplot2::aes(week_id, inc100k_median)
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = inc100k_q25, ymax = inc100k_q75),
    fill = "lightgray", alpha = 0.8
  ) +
  ggplot2::geom_line(colour = "steelblue") +
  ggplot2::facet_wrap(
    ggplot2::vars(koppen), scales = "free_y"
  )






