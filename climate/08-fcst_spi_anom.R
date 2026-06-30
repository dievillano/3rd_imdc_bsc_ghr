# Script name: 08_fcst_spi_anom.R
#
# Purpose of script: Calculate SPEI and tas anomalies for brazil forecast
# Author: Carles Milà
# Date Created: 2025-07-29

library("SPEI")
library("dplyr")
library("ggplot2")
library("zoo")

# Read and create datasets ----
monthly_adm <- read.csv("temp_files/sprint_datasets_tas_prlr_enso.csv") |> 
  mutate(date = as.Date(date))
monthly_adm <- monthly_adm[order(monthly_adm$regional_geocode, monthly_adm$year, monthly_adm$month), ]

val1 <- monthly_adm[c("regional_geocode", "adm_latitude", "month", "year", "date", 
                      "tas_val1", "prlr_val1", "nino34_val1")] |> 
  rename(tas = tas_val1, prlr = prlr_val1, nino34 = nino34_val1)
val2 <- monthly_adm[c("regional_geocode", "adm_latitude", "month", "year", "date", 
                      "tas_val2", "prlr_val2", "nino34_val2")] |> 
  rename(tas = tas_val2, prlr = prlr_val2, nino34 = nino34_val2)
val3 <- monthly_adm[c("regional_geocode", "adm_latitude", "month", "year", "date", 
                      "tas_val3", "prlr_val3", "nino34_val3")] |> 
  rename(tas = tas_val3, prlr = prlr_val3, nino34 = nino34_val3)
fcst <- monthly_adm[c("regional_geocode", "adm_latitude", "month", "year", "date", 
                      "tas_fcst", "prlr_fcst", "nino34_fcst")] |> 
  rename(tas = tas_fcst, prlr = prlr_fcst, nino34 = nino34_fcst)

# tas6 ----
compute_tas6 <- function(df){
  df <- df |> 
    group_by(regional_geocode) %>%
    arrange(date) %>%
    mutate(tas6 = rollmean(tas, k = 6, fill = NA, align = "right")) |> 
    ungroup() |> 
    arrange(regional_geocode, date)
}

val1 <- compute_tas6(val1)
val2 <- compute_tas6(val2)
val3 <- compute_tas6(val3)
fcst <- compute_tas6(fcst)

sapply(val3, function(x) sum(is.na(x)))
ggplot(val3[val3$regional_geocode==val3$regional_geocode[1],]) +
  geom_line(aes(x = as.Date(date), y = tas), col = "black") +
  geom_line(aes(x = as.Date(date), y = tas6), col = "red")

# oni ----
compute_oni <- function(df){
  df <- df |> 
    group_by(regional_geocode) %>%
    arrange(date) %>%
    mutate(oni = rollmean(nino34, k = 3, fill = NA, align = "right")) |> 
    ungroup() |> 
    arrange(regional_geocode, date)
}

val1 <- compute_oni(val1)
val2 <- compute_oni(val2)
val3 <- compute_oni(val3)
fcst <- compute_oni(fcst)

sapply(val3, function(x) sum(is.na(x)))
ggplot(val3[val3$regional_geocode==val3$regional_geocode[1],]) +
  geom_line(aes(x = as.Date(date), y = nino34), col = "black") +
  geom_line(aes(x = as.Date(date), y = oni), col = "red")

# tasan6, spei3, spei12 ------------------------------------------------------

# Reference period (1981-2010)
ref_year1 <- 1981 
ref_year2 <- 2010 

# Computations
source("R/utils.R")

val1_all <- data.frame()
val2_all <- data.frame()
val3_all <- data.frame()
fcst_all <- data.frame()

for(adm_id in unique(monthly_adm$regional_geocode)){
  
  # Subset of data
  val1_id <- val1[val1$regional_geocode == adm_id,]
  val2_id <- val2[val2$regional_geocode == adm_id,]
  val3_id <- val3[val3$regional_geocode == adm_id,]
  fcst_id <- fcst[fcst$regional_geocode == adm_id,]
  
  # TS tas
  val1_tas6_ts <- ts(val1_id$tas6, start = c(1950, 1), frequency = 12)
  val2_tas6_ts <- ts(val2_id$tas6, start = c(1950, 1), frequency = 12)
  val3_tas6_ts <- ts(val3_id$tas6, start = c(1950, 1), frequency = 12)
  fcst_tas6_ts <- ts(fcst_id$tas6, start = c(1950, 1), frequency = 12)
  
  # pet
  val1_id$pet <- thornthwaite(val1_id$tas, val1_id$adm_latitude[1], verbose = FALSE)
  val2_id$pet <- thornthwaite(val2_id$tas, val2_id$adm_latitude[1], verbose = FALSE)
  val3_id$pet <- thornthwaite(val3_id$tas, val3_id$adm_latitude[1], verbose = FALSE)
  fcst_id$pet <- thornthwaite(fcst_id$tas, fcst_id$adm_latitude[1], verbose = FALSE)
  
  # TS prlr
  val1_prlr_ts <- ts(val1_id$prlr-val1_id$pet, start = c(1950, 1), frequency = 12)
  val2_prlr_ts <- ts(val2_id$prlr-val2_id$pet, start = c(1950, 1), frequency = 12)
  val3_prlr_ts <- ts(val3_id$prlr-val3_id$pet, start = c(1950, 1), frequency = 12)
  fcst_prlr_ts <- ts(fcst_id$prlr-fcst_id$pet, start = c(1950, 1), frequency = 12)
  
  # Temp anomalies
  val1_tasan6 <- monthly_temp_anomaly(val1_tas6_ts, ref.start = c(ref_year1, 1), ref.end = c(ref_year2, 12))
  val2_tasan6 <- monthly_temp_anomaly(val2_tas6_ts, ref.start = c(ref_year1, 1), ref.end = c(ref_year2, 12))
  val3_tasan6 <- monthly_temp_anomaly(val3_tas6_ts, ref.start = c(ref_year1, 1), ref.end = c(ref_year2, 12))
  fcst_tasan6 <- monthly_temp_anomaly(fcst_tas6_ts, ref.start = c(ref_year1, 1), ref.end = c(ref_year2, 12))
  
  # spei3
  val1_spei3 <- spei(val1_prlr_ts, scale = 3, ref.end = c(ref_year2, 12), verbose = FALSE)
  val2_spei3 <- spei(val2_prlr_ts, scale = 3, ref.end = c(ref_year2, 12), verbose = FALSE)
  val3_spei3 <- spei(val3_prlr_ts, scale = 3, ref.end = c(ref_year2, 12), verbose = FALSE)
  fcst_spei3 <- spei(fcst_prlr_ts, scale = 3, ref.end = c(ref_year2, 12), verbose = FALSE)
  
  # spei12
  val1_spei12 <- spei(val1_prlr_ts, scale = 12, ref.end = c(ref_year2, 12), verbose = FALSE)
  val2_spei12 <- spei(val2_prlr_ts, scale = 12, ref.end = c(ref_year2, 12), verbose = FALSE)
  val3_spei12 <- spei(val3_prlr_ts, scale = 12, ref.end = c(ref_year2, 12), verbose = FALSE)
  fcst_spei12 <- spei(fcst_prlr_ts, scale = 12, ref.end = c(ref_year2, 12), verbose = FALSE)

  # Parse and trim Inf values
  trim_vals <- function(x){
    v <- c(x)
    ifelse(v==Inf, 6, ifelse(v==-Inf, -6, v))
  }
  
  # Write results
  val1_id$tasan6 <- trim_vals(val1_tasan6$anomaly)
  val2_id$tasan6 <- trim_vals(val2_tasan6$anomaly)
  val3_id$tasan6 <- trim_vals(val3_tasan6$anomaly)
  fcst_id$tasan6 <- trim_vals(fcst_tasan6$anomaly)
  
  val1_id$spei3 <- trim_vals(fitted(val1_spei3))
  val2_id$spei3 <- trim_vals(fitted(val2_spei3))
  val3_id$spei3 <- trim_vals(fitted(val3_spei3))
  fcst_id$spei3 <- trim_vals(fitted(fcst_spei3))
  
  val1_id$spei12 <- trim_vals(fitted(val1_spei12))
  val2_id$spei12 <- trim_vals(fitted(val2_spei12))
  val3_id$spei12 <- trim_vals(fitted(val3_spei12))
  fcst_id$spei12 <- trim_vals(fitted(fcst_spei12))
  
  # progress
  val1_all <- rbind(val1_all, val1_id)
  val2_all <- rbind(val2_all, val2_id)
  val3_all <- rbind(val3_all, val3_id)
  fcst_all <- rbind(fcst_all, fcst_id)
  
  print(paste0(match(adm_id, unique(val1$regional_geocode)),
               "/", length(unique(val1$regional_geocode))))
}


# Write to disk ----
val1_all <- val1_all[val1_all$year>=2009,] |> 
  select(-pet, -adm_latitude, -tas, -prlr, -nino34)
sapply(val1_all, function(x) sum(is.na(x)))
write.csv(val1_all, "out_files/clim_validation1.csv", row.names = FALSE)

val2_all <- val2_all[val2_all$year>=2009,] |> 
  select(-pet, -adm_latitude, -tas, -prlr, -nino34)
sapply(val2_all, function(x) sum(is.na(x)))
write.csv(val2_all, "out_files/clim_validation2.csv", row.names = FALSE)

val3_all <- val3_all[val3_all$year>=2009,] |> 
  select(-pet, -adm_latitude, -tas, -prlr, -nino34)
sapply(val3_all, function(x) sum(is.na(x)))
write.csv(val3_all, "out_files/clim_validation3.csv", row.names = FALSE)

fcst_all <- fcst_all[fcst_all$year>=2009,] |> 
  select(-pet, -adm_latitude, -tas, -prlr, -nino34)
sapply(fcst_all, function(x) sum(is.na(x)))
write.csv(fcst_all, "out_files/clim_forecast.csv", row.names = FALSE)
