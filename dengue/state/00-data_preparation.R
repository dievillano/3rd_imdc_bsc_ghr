sprint2026_path <- here::here()

data_path <- fs::path(sprint2026_path, "data")
raw_data_path <- fs::path(data_path, "raw/data_imdc_2026")
raw_data_filepaths <- fs::dir_ls(raw_data_path)
names(raw_data_filepaths) <- fs::path_file(raw_data_filepaths)

climate_path  <- fs::path(sprint2026_path, "climate")
climate_out_path  <- fs::path(climate_path, "out_files")

interim_data_path <- fs::path(data_path, "interim/health_region")
processed_data_path <- fs::path(data_path, "processed/health_region")
resources_path <- fs::path("resources/health_region")
shp_path <- fs::path(resources_path, "shp")
graph_path <- fs::path(resources_path, "graph")

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

# 1. Join table -----------------------------------------------------------

map_regional_health_raw <- readr::read_csv(
  raw_data_filepaths["map_regional_health.csv"],
  col_types = "iciccicicic"
)

cat(
  sprintf(
    paste0(
      "Number of municipalities: %s ",
      "\nNumber of health regions: %s ",
      "\nNumber of health macro-regions: %s ",
      "\nNumber of states: %s ",
      "\nNumber of macro-regions: %s "
    ), 
    nrow(map_regional_health_raw), 
    dplyr::n_distinct(map_regional_health_raw$regional_geocode),
    dplyr::n_distinct(map_regional_health_raw$macroregional_geocode), 
    dplyr::n_distinct(map_regional_health_raw$uf_code),
    dplyr::n_distinct(map_regional_health_raw$macroregion_code)
  )
)

drop_state <- 32

geocode_regional_uf_join <- map_regional_health_raw |> 
  dplyr::filter(uf_code != drop_state) |> 
  dplyr::select(uf, uf_code, regional_geocode, geocode)

# 2. Shape files ----------------------------------------------------------

# Municipalities
muni_shp_raw <- sf::read_sf(
  raw_data_filepaths["shape_muni.gpkg"]
)

muni_shp <- muni_shp_raw |> 
  dplyr::filter(uf_code != drop_state) |> 
  dplyr::mutate(
    geocode = as.integer(geocode),
    uf_code = as.integer(uf_code)
  )

ggplot2::ggplot() +
  ggplot2::geom_sf(data = muni_shp)

# Health regions
hr_shp_raw <- sf::read_sf(
  raw_data_filepaths["shape_regional_health.gpkg"]
)

hr_shp <- hr_shp_raw |> 
  dplyr::mutate(
    regional_geocode = as.integer(regional_geocode),
    uf_code = as.integer(uf_code)
  ) |> 
  dplyr::filter(uf_code != drop_state)

ggplot2::ggplot() +
  ggplot2::geom_sf(data = hr_shp)

saveRDS(hr_shp, fs::path(shp_path, "hr_shapefile.rds"))

hr_shp_clean <- hr_shp |>
  sf::st_make_valid() |> 
  sf::st_transform(5880)

hr_nb <- spdep::poly2nb(hr_shp_clean, queen = TRUE)

spdep::nb2INLA(
  file = fs::path(graph_path, "hr_graph.graph"),
  hr_nb
)

# Health macro-regions
hmr_shp_raw <- sf::read_sf(
  raw_data_filepaths["shape_macroregional_health.gpkg"]
)

hmr_shp <- hmr_shp_raw |> 
  dplyr::mutate(
    macroregional_geocode = as.integer(macroregional_geocode),
    uf_code = as.integer(uf_code)
  ) |> 
  dplyr::filter(uf_code != drop_state)

ggplot2::ggplot() +
  ggplot2::geom_sf(data = hmr_shp)

# 3. Dengue dataset -------------------------------------------------------

dengue_raw <- readr::read_csv(
  file = raw_data_filepaths["dengue.csv.gz"],
  col_types = "iDiiciiilllllllllc"
)

dengue_raw <- dengue_raw |> 
  dplyr::filter(uf_code != drop_state)

validate_weekly_panel(
  data = dengue_raw,
  unit = geocode,
  date = date,
  epiweek = epiweek,
  spatial_units = geocode_regional_uf_join$geocode,
  fail = "warning"
)

dengue_hr <- dengue_raw |>
  dplyr::group_by(uf_code, uf, regional_geocode, date, epiweek) |>
  dplyr::summarise(
    cases = sum(casos),
    across(tidyselect::starts_with("train_"), dplyr::first),
    across(tidyselect::starts_with("target_"), dplyr::first),
    .groups = "drop"
  ) |> 
  dplyr::select(-target_city)

validate_weekly_panel(
  data = dengue_hr,
  unit = regional_geocode,
  date = date,
  epiweek = epiweek,
  spatial_units = unique(geocode_regional_uf_join$regional_geocode),
  fail = "warning"
)

rm(dengue_raw)
gc()

saveRDS(dengue_hr, fs::path(interim_data_path, "dengue_hr.rds"))

rm(dengue_hr)
gc()

# 4. Population dataset ---------------------------------------------------

population_raw <- readr::read_csv(
  file = raw_data_filepaths["datasus_population_2001_2025.csv.gz"],
  col_types = "iii"
)

population_2026 <- population_raw |>
  dplyr::bind_rows(
    population_raw |>
      dplyr::distinct(geocode) |>
      dplyr::mutate(
        year = 2026,
        population = NA_real_
      )
  ) |>
  dplyr::arrange(geocode, year) |>
  dplyr::group_by(geocode) |>
  dplyr::mutate(
    population = zoo::na.approx(
      population,
      x = year,
      na.rm = FALSE,
      rule = 2
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(year == 2026)

population <- population_raw |> 
  dplyr::bind_rows(population_2026) |> 
  dplyr::inner_join(geocode_regional_uf_join, by = "geocode") |> 
  dplyr::filter(uf_code != drop_state) |> 
  dplyr::select(uf_code, uf, regional_geocode, geocode, year, population)

population_hr <- population |> 
  dplyr::group_by(uf_code, uf, regional_geocode, year) |> 
  dplyr::summarise(
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(population_hr, fs::path(interim_data_path, "population_hr.rds"))

# 5. Climate dataset ------------------------------------------------------

# climate_raw <- readr::read_csv(
#   file = raw_data_filepaths["climate.csv.gz"],
#   col_types = "Diidddddddddddddi"
# )

# climate_raw <- climate_raw |> 
#   dplyr::inner_join(geocode_regional_uf_join, by = "geocode") |> 
#   dplyr::filter(uf_code != drop_state)

# validate_weekly_panel(
#   data = climate_raw,
#   unit = geocode,
#   date = date,
#   spatial_units = geocode_regional_uf_join$geocode,
#   fail = "warning"
# )

# climate_vars <- c(
#   "temp_min", 
#   "temp_med", 
#   "temp_max",        
#   "precip_min", 
#   "precip_med", 
#   "precip_max", 
#   "pressure_min", 
#   "pressure_med", 
#   "pressure_max", 
#   "rel_humid_min", 
#   "rel_humid_med", 
#   "rel_humid_max", 
#   "thermal_range", 
#   "rainy_days"
# )

# climate_pop_hr <- climate_raw |> 
#   dplyr::mutate(year = lubridate::year(date)) |> 
#   dplyr::filter(year >= 2001) |> 
#   dplyr::left_join(
#     population, 
#     by = c("uf_code", "uf", "regional_geocode", "geocode", "year")
#   ) |> 
#   dplyr::group_by(uf_code, uf, regional_geocode, date, epiweek) |> 
#   dplyr::summarise(
#     dplyr::across(
#       tidyselect::all_of(climate_vars),
#       \(x) weighted.mean(x, population, na.rm = TRUE),
#       .names = "{.col}_weight"
#     ),
#     .groups = "drop"
#   ) |> 
#   dplyr::mutate(
#     epiweek_num = as.integer(substr(epiweek, 5, 6)),
#     epiweek_num = dplyr::if_else(epiweek_num == 53L, 52L, epiweek_num),
#     year = lubridate::year(date)
#   )
  
# rm(climate_raw)
# gc()

# climate_pop_hr_colnames <- colnames(climate_pop_hr)
# anom_vars <- climate_pop_hr_colnames[
#   grepl("^(temp|precip|rel_humid).*_weight$", climate_pop_hr_colnames)
# ]

# climate_hr_climatology <- climate_pop_hr |>
#   dplyr::filter(dplyr::between(year, 2001, 2020)) |>
#   dplyr::group_by(uf_code, uf, regional_geocode, epiweek_num) |>
#   dplyr::summarise(
#     dplyr::across(
#       tidyselect::all_of(anom_vars),
#       \(x) mean(x, na.rm = TRUE),
#       .names = "{.col}_base"
#     ),
#     .groups = "drop"
#   )  
  
# climate_hr_raw <- climate_pop_hr |>
#   dplyr::left_join(
#     climate_hr_climatology,
#     by = c("uf_code", "uf", "regional_geocode", "epiweek_num")
#   ) 
  
# for (var in anom_vars) {
#   climate_hr_raw[[paste0(var, "_anom")]] <-
#     climate_hr_raw[[var]] - climate_hr_raw[[paste0(var, "_base")]]
# }  
  
# climate_hr <- climate_hr_raw |> 
#   dplyr::filter(year >= 2009) |> 
#   dplyr::select(-tidyselect::ends_with("_base"))
  
# validate_weekly_panel(
#   data = climate_hr,
#   unit = regional_geocode,
#   date = date,
#   epiweek = epiweek,
#   spatial_units = unique(geocode_regional_uf_join$regional_geocode),
#   fail = "warning"
# )  
  
# saveRDS(climate_hr, fs::path(interim_data_path, "climate_hr.rds"))

# rm(climate_pop_hr, climate_hr_raw, climate_hr)
# gc()

#---

climate_raw_colnames  <- c(
  "month", 
  "year", 
  "regional_geocode", 
  "regional_name", 
  "prlr", 
  "tas", 
  "tasmin", 
  "tasmax", 
  "tasan1", 
  "tasan3", 
  "tasan6", 
  "tasan12", 
  "spi1", 
  "spi3", 
  "spi6", 
  "spi12", 
  "spei1", 
  "spei3", 
  "spei6", 
  "spei12", 
  "nino", 
  "oni"
)

climate_hr <- readr::read_csv(
  file = fs::path(climate_out_path, "brazil_monthly_hist_200801-202512.csv"),
  col_names = climate_raw_colnames,
  col_types = "iiicdddddddddddddddddd",
  skip = 1,
  col_select = -c(regional_name)
)

saveRDS(climate_hr, fs::path(interim_data_path, "climate_hr.rds"))

rm(climate_hr)
gc()

# Climate forecasts

forecast_files <- c(
  validation_1 = "clim_validation1.csv",
  validation_2 = "clim_validation2.csv",
  validation_3 = "clim_validation3.csv",
  validation_4 = "clim_forecast.csv"
)

for (i in seq_along(forecast_files)) {
  forecast_df  <- readr::read_csv(
    file = fs::path(climate_out_path, forecast_files[i]),
    col_types = "iiiDddddd",
    col_select = -c(date)
  )

  saveRDS(forecast_df, fs::path(interim_data_path, paste0("climate_forecasts_val_", i, ".rds")))

}

# 6. Environmental dataset ------------------------------------------------

environ_raw <- readr::read_csv(
  file = raw_data_filepaths["environ_vars.csv.gz"],
  col_types = "iicc"
)

environ_raw <- environ_raw |> 
  dplyr::filter(uf_code != drop_state) |> 
  dplyr::left_join(geocode_regional_uf_join, by = c("geocode", "uf_code"))

environ_hr <- environ_raw |> 
  dplyr::group_by(uf_code, uf, regional_geocode) |> 
  dplyr::summarise(
    biome = names(sort(table(biome), decreasing = TRUE))[1],
    koppen = names(sort(table(koppen), decreasing = TRUE))[1],
    .groups = "drop"
  )

saveRDS(environ_hr, fs::path(interim_data_path, "environ_hr.rds"))

# 7. Ocean temperature and level oscillations -----------------------------

# ocean_raw <- readr::read_csv(
#   file = raw_data_filepaths["ocean_climate_oscillations.csv.gz"],
#   col_types = "Ddddi"
# )

# saveRDS(ocean_raw, fs::path(interim_data_path, "ocean.rds"))


