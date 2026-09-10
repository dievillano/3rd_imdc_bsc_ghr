# Script name: 07_fcst_merge.R
#
# Purpose of script: Create validation and forecast climatic datasets
# Author: Carles Milà
# Date Created: 2025-07-29
#
# Env: HUB with module R-bundle-CRAN/2024.06-foss-2023b

library("dplyr")
library("ggplot2")
library("tidyr")
library("lubridate")

# Read data  ----

## Observed clim ----
obs <- read.csv("temp_files/brazil_healthregions_tas_prlr_195001-202512.csv") 
obs <- obs[c("adm_id", "adm_latitude",  "year", "month", "tas", "prlr")] |> 
  rename("regional_geocode" = "adm_id",
         "tas_obs" = "tas",
         "prlr_obs" = "prlr")

## Observed enso ----
enso <- read.csv("out_files/brazil_monthly_hist_nino_200801-202505.csv") |> 
  select(year, month, nino34) |> 
  rename("nino34_obs" = "nino34")
obs <- left_join(obs, enso, by = c("year", "month"))

## Forecast clim ----
load_forecasts <- function(tasfile, prlrfile, year){
  
  # Read data
  tasdata <- read.csv(tasfile) |> 
    group_by(regional_geocode, leadtime) |> 
    summarise(tas = mean(mean), .groups = "keep") |> # ensemble mean
    ungroup()
  names(tasdata) <- c("regional_geocode", "leadtime", paste0("tas_", year))
  prlrdata <- read.csv(prlrfile) |> 
    group_by(regional_geocode, leadtime) |> 
    summarise(prlr = mean(mean), .groups = "keep") |> # ensemble mean
    ungroup()
  names(prlrdata) <- c("regional_geocode", "leadtime", paste0("prlr_", year))
  fcstdata <- full_join(tasdata, prlrdata, 
                        by = c("regional_geocode", "leadtime"))
  
  # Add temporal identifiers
  fcstdata$year <- year
  fcstdata$month <- fcstdata$leadtime + 5 # Only valid for June!
  fcstdata$leadtime <- NULL
  
  return(fcstdata)
}

fcst2022 <- load_forecasts("out_files/brazil_monthly_fcst_202206_tas.csv",
                           "out_files/brazil_monthly_fcst_202206_prlr.csv", 
                           2022)
fcst2023 <- load_forecasts("out_files/brazil_monthly_fcst_202306_tas.csv",
                           "out_files/brazil_monthly_fcst_202306_prlr.csv", 
                           2023)
fcst2024 <- load_forecasts("out_files/brazil_monthly_fcst_202406_tas.csv",
                           "out_files/brazil_monthly_fcst_202406_prlr.csv", 
                           2024)
fcst2025 <- load_forecasts("out_files/brazil_monthly_fcst_202506_tas.csv",
                           "out_files/brazil_monthly_fcst_202506_prlr.csv", 
                           2025)

## Forecast enso ----
load_enso <- function(ensopath){
  enso <-  read.csv(ensopath) |> 
    select(-month) |> 
    rename(year = syear, month = fmonth) |> 
    mutate(month = lubridate::month(as.Date(month))) |> 
    group_by(year, month) |> 
    summarise(nino34 = mean(Nino34), .groups = "keep")
  enso
}
enso2022 <- load_enso("nino/brazil_monthly_fcst_202206_ElNino.csv") |> 
  rename(nino34_2022 = nino34)
enso2023 <- load_enso("nino/brazil_monthly_fcst_202306_ElNino.csv") |> 
  rename(nino34_2023 = nino34)
enso2024 <- load_enso("nino/brazil_monthly_fcst_202406_ElNino.csv") |> 
  rename(nino34_2024 = nino34)
enso2025 <- load_enso("nino/brazil_monthly_fcst_202506_ElNino.csv") |> 
  rename(nino34_2025 = nino34)

## Climatologies ----
clims <- read.csv("temp_files/climatologies_199101-202012.csv") |> 
  rename("regional_geocode" = "adm_id",
         "tas_clim" = "tas",
         "prlr_clim" = "prlr",
         "nino34_clim" = "nino34")


# Concatenate datasets ----

# Space-time grid
allclim <- expand.grid(regional_geocode = unique(obs$regional_geocode),
                       month = 1:12,
                       year = 1950:2026) |> 
  mutate(date = as.Date(paste(year, sprintf("%02d", month), "01", sep = "-")))

# Merge datasets
allclim <- allclim |> 
  left_join(obs, by = c("regional_geocode", "month", "year")) |> 
  left_join(fcst2022, by = c("regional_geocode", "month", "year")) |> 
  left_join(fcst2023, by = c("regional_geocode", "month", "year")) |> 
  left_join(fcst2024, by = c("regional_geocode", "month", "year")) |> 
  left_join(fcst2025, by = c("regional_geocode", "month", "year")) |>
  left_join(enso2022, by = c("month", "year")) |> 
  left_join(enso2023, by = c("month", "year")) |> 
  left_join(enso2024, by = c("month", "year")) |> 
  left_join(enso2025, by = c("month", "year")) |>
  left_join(clims, by = join_by("regional_geocode", "month"))
  

# tas ----

# Validation 1
allclim$tas_val1 <- case_when(
  allclim$date < as.Date("2022-06-01") ~ allclim$tas_obs,
  allclim$date < as.Date("2023-01-01") ~ allclim$tas_2022,
  .default = allclim$tas_clim
)
sum(is.na(allclim$tas_val1))

# Validation 2
allclim$tas_val2 <- case_when(
  allclim$date < as.Date("2023-06-01") ~ allclim$tas_obs,
  allclim$date < as.Date("2024-01-01") ~ allclim$tas_2023,
  .default = allclim$tas_clim
)
sum(is.na(allclim$tas_val2))

# Validation 3
allclim$tas_val3 <- case_when(
  allclim$date < as.Date("2024-06-01") ~ allclim$tas_obs,
  allclim$date < as.Date("2025-01-01") ~ allclim$tas_2024,
  .default = allclim$tas_clim
)
sum(is.na(allclim$tas_val3))

# Forecast
allclim$tas_fcst <- case_when(
  allclim$date < as.Date("2025-06-01") ~ allclim$tas_obs,
  allclim$date < as.Date("2026-01-01") ~ allclim$tas_2025,
  .default = allclim$tas_clim
)
sum(is.na(allclim$tas_fcst))

# Plot
allclim |> 
  select(date, regional_geocode, tas_val1, tas_val2, tas_val3, tas_fcst) |> 
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2026-12-01")) |> 
  filter(regional_geocode == obs$regional_geocode[1]) |> 
  pivot_longer(cols = 3:6, names_to = "dataset", values_to = "tas") |> 
  ggplot() + geom_line(aes(x = date, y = tas, col = dataset, group = dataset)) +
  scale_x_date(breaks = "6 month") +
  theme_bw()


# prlr ----

# Validation 1
allclim$prlr_val1 <- case_when(
  allclim$date < as.Date("2022-06-01") ~ allclim$prlr_obs,
  allclim$date < as.Date("2023-01-01") ~ allclim$prlr_2022,
  .default = allclim$prlr_clim
)
sum(is.na(allclim$prlr_val1))

# Validation 2
allclim$prlr_val2 <- case_when(
  allclim$date < as.Date("2023-06-01") ~ allclim$prlr_obs,
  allclim$date < as.Date("2024-01-01") ~ allclim$prlr_2023,
  .default = allclim$prlr_clim
)
sum(is.na(allclim$prlr_val2))

# Validation 3
allclim$prlr_val3 <- case_when(
  allclim$date < as.Date("2024-06-01") ~ allclim$prlr_obs,
  allclim$date < as.Date("2025-01-01") ~ allclim$prlr_2024,
  .default = allclim$prlr_clim
)
sum(is.na(allclim$prlr_val3))

# Forecast
allclim$prlr_fcst <- case_when(
  allclim$date < as.Date("2025-06-01") ~ allclim$prlr_obs,
  allclim$date < as.Date("2026-01-01") ~ allclim$prlr_2025,
  .default = allclim$prlr_clim
)
sum(is.na(allclim$prlr_fcst))

# Plot
allclim |> 
  select(date, regional_geocode, prlr_val1, prlr_val2, prlr_val3, prlr_fcst) |> 
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2026-12-01")) |> 
  filter(regional_geocode == obs$regional_geocode[1]) |> 
  pivot_longer(cols = 3:6, names_to = "dataset", values_to = "prlr") |> 
  ggplot() + geom_line(aes(x = date, y = prlr, col = dataset, group = dataset),
                       alpha = 0.7) +
  scale_x_date(breaks = "6 month") +
  theme_bw()


# enso ----

# Validation 1
allclim$nino34_val1 <- case_when(
  allclim$date < as.Date("2022-06-01") ~ allclim$nino34_obs,
  allclim$date < as.Date("2022-12-01") ~ allclim$nino34_2022,
  .default = allclim$nino34_clim
)
sum(is.na(allclim$nino34_val1[allclim$date>as.Date("2018-01-01")]))

# Validation 2
allclim$nino34_val2 <- case_when(
  allclim$date < as.Date("2023-06-01") ~ allclim$nino34_obs,
  allclim$date < as.Date("2023-12-01") ~ allclim$nino34_2023,
  .default = allclim$nino34_clim
)
sum(is.na(allclim$nino34_val2[allclim$date>as.Date("2018-01-01")]))

# Validation 3
allclim$nino34_val3 <- case_when(
  allclim$date < as.Date("2024-06-01") ~ allclim$nino34_obs,
  allclim$date < as.Date("2024-12-01") ~ allclim$nino34_2024,
  .default = allclim$nino34_clim
)
sum(is.na(allclim$nino34_val3[allclim$date>as.Date("2018-01-01")]))

# Forecast
allclim$nino34_fcst <- case_when(
  allclim$date < as.Date("2025-06-01") ~ allclim$nino34_obs,
  allclim$date < as.Date("2025-12-01") ~ allclim$nino34_2025,
  .default = allclim$nino34_clim
)
sum(is.na(allclim$nino34_fcst[allclim$date>as.Date("2018-01-01")]))

# Plot
allclim |> 
  select(date, regional_geocode, nino34_val1, nino34_val2, nino34_val3, nino34_fcst) |> 
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2026-12-01")) |> 
  filter(regional_geocode == obs$regional_geocode[1]) |> 
  pivot_longer(cols = 3:6, names_to = "dataset", values_to = "nino34") |> 
  ggplot() + geom_line(aes(x = date, y = nino34, col = dataset, group = dataset),
                       alpha = 0.7) +
  scale_x_date(breaks = "6 month") +
  theme_bw()


# Write to disk ----
allclim <- select(allclim,
                  regional_geocode, adm_latitude, month, year, date,
                  contains("_val"), contains("_fcst"))
write.csv(allclim, "temp_files/sprint_datasets_tas_prlr_enso.csv",
          row.names = FALSE)
