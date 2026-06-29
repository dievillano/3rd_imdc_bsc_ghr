# Script name: 02_obs_spi
#
# Calculate SPI and SPEI for brazil
# 
# Author: Carles Milà (adapted from Daniela Lührsen's script)
# Date Created: 2025-06-03
# Email: carles.milagarcia@bsc.es
#
# Env: HUB with module R-bundle-CRAN/2024.06-foss-2023b


# Project root
# library("here")
# here::i_am("R/01_obs_tas_prlr.R")

## load packages
packages <- c("startR", "s2dv", "CSTools", "multiApply", "ClimProjDiags",
              "SPEI", "zoo", "TLMoments", "lmomco", "lmom", "lubridate")
lapply(packages, require, character.only = TRUE)

# Load local packages
library("exactextractr", lib = "/home/Earth/cmilagar/packages")
source("climate/utils.R")

# Setup ---------------------------

# select parameters:
dataset <- "era5land" # options "era5", "era5land", "chirps"
start_year <- 1950
end_year <- 2025
start_month <- 1
end_month <- 12

lats_min <- -35
lats_max <- 15
lons_min <- -80
lons_max <- -30

# obtain dates for startR call:
# for (mm in start_month:end_month){
#   if (mm == start_month) {
#     dates <- as.Date(paste0(start_year:end_year, "-", start_month, "-1"))
#   } else {
#     dates <- append(dates, as.Date(paste0(start_year:end_year, "-", mm, "-1")))
#   }
# }
# dates <- dates[order(dates)]
# dates <- array(substr(gsub("-", "", as.character(dates)), 1, 6),
#                dim = c(time = (end_month - start_month + 1),
#                        syear = (end_year - start_year + 1)))
period <- paste0(start_year, sprintf("%02d", start_month), "-",
                 end_year, sprintf("%02d", end_month))

# Prep precipitation ----

# obtain path for startR call:
path_dataset <- "/esarchive/recon/ecmwf/era5land/monthly_mean/$var$_f1h/$var$_$sdate$.nc"

# startR call:
# data_prlr <- startR::Start(dat = path_dataset,
#                           var = "prlr",
#                           sdate = dates,
#                           split_multiselected_dims = TRUE,
#                           latitude = startR::values(list(lats_min, lats_max)),
#                           latitude_reorder = startR::Sort(decreasing = TRUE),
#                           longitude = startR::values(list(lons_min, lons_max)),
#                           longitude_reorder = startR::CircularSort(-180, 180),
#                           synonims = list(latitude = c("lat", "latitude"),
#                                           longitude = c("lon", "longitude")),
#                           return_vars = list(latitude = "dat",
#                                               longitude = "dat",
#                                               time = c("sdate")),
#                           retrieve = TRUE)
# data_prlr <- data_prlr * 3600 * 24 * 30.44 * 1000
# attr(data_prlr, "Variables")$common$prlr$units <- "mm"
# data_prlr <- CSTools::as.s2dv_cube(data_prlr)
# if (!("ensemble" %in% names(data_prlr$data))) {
#   data_prlr$data <- s2dv::InsertDim(data_prlr$data, pos =  5, len = 1, name = "ensemble")
# }
# 
# saveRDS(data_prlr, paste0("climate/out_files/brazil_prlr_", period, "_t1.rds"))
# rm("data_prlr")

get_prlr_chunk(1950, 1979)
get_prlr_chunk(1980, 2009)
get_prlr_chunk(2010, 2025)


# Prep tas ----
path_dataset <- "/esarchive/recon/ecmwf/era5land/monthly_mean/$var$_f1h/$var$_$sdate$.nc"

# startR call:
# data_tas <- startR::Start(dat = path_dataset,
#                           var = "tas",
#                           sdate = dates,
#                           split_multiselected_dims = TRUE,
#                           latitude = startR::values(list(lats_min, lats_max)),
#                           latitude_reorder = startR::Sort(decreasing = TRUE),
#                           longitude = startR::values(list(lons_min, lons_max)),
#                           longitude_reorder = startR::CircularSort(-180, 180),
#                           synonims = list(latitude = c("lat", "latitude"),
#                                           longitude = c("lon", "longitude")),
#                           return_vars = list(latitude = "dat",
#                                             longitude = "dat",
#                                             time = c("sdate")),
#                           retrieve = TRUE)
# data_tas <- data_tas - 273.15
# attr(data_tas, "Variables")$common$tas$units <- "C"
# data_tas <- CSTools::as.s2dv_cube(data_tas)
# if (!("ensemble" %in% names(data_tas$data))) {
#   data_tas$data <- s2dv::InsertDim(data_tas$data, pos =  5, len = 1, name = "ensemble")
# }
# saveRDS(data_tas, paste0("climate/out_files/brazil_tas_", period, "_t1.rds"))
# rm("data_tas")

get_tas_chunk(1950, 1979)
get_tas_chunk(1980, 2009)
get_tas_chunk(2010, 2025)

# Convert to raster -------------------------------------------------------
# data_prlr <- readRDS(paste0("climate/out_files/brazil_prlr_", period, "_t1.rds"))
# data_tas <- readRDS(paste0("climate/out_files/brazil_tas_", period, "_t1.rds"))
# 
# datas <- list(data_prlr, data_tas)
# 
# for (data in datas){
#   print(data$attrs$Variable$varName)
# 
#   # Assign latitude and longitude coordinates from data
#   lon <- data$coords$longitude
#   lat <- data$coords$latitude
# 
#   # Set time variables
#   nmonths <- data$dims[["time"]]
#   nyears <- data$dims[["syear"]]
#   ntimes <- nyears * nmonths
# 
#   # Transform multidimensional array into list of rasters
#   out <- NULL
#   for (x in 1:ntimes){
# 
#     # Calculate i and j from x
#     m <- ((x - 1) %% nmonths) + 1
#     y <- (((x - 1) %/% nmonths)) %% nyears + 1
# 
#     # Extract date
#     b <- substr(data$attrs$Dates[x], 1, 10)
# 
#     if (is.na(b)) {
#       print(paste(x, b))
#       next
#     }
#     # Extract array of values for year-month-day combination
#     a <-  data$data[1, 1, m, y, 1, , ] #month, year, day
# 
#     # Convert to a raster
#     out[[b]] <- raster::raster(a,
#                                xmn = min(lon), xmx = max(lon),
#                                ymn = min(lat), ymx = max(lat))
#     # Assign CRS
#     raster::crs(out[[b]]) <- "EPSG:4326"
#     print(x)
#   }
# 
#   # Convert list of rasters into stack
#   assign(paste0(data$attrs$Variable$varName, "_raster"), NULL)
#   assign(paste0(data$attrs$Variable$varName, "_raster"), raster::stack(out))
#   print(data$attrs$Variable$varName)
# }
# 
# saveRDS(prlr_raster, paste0("climate/out_files/brazil_prlr_", period, "_t2.rds"))
# saveRDS(tas_raster, paste0("climate/out_files/brazil_tas_", period, "_t2.rds"))
# rm("prlr_raster", "tas_raster")

prlr_files <- c(
  "climate/out_files/brazil_prlr_195001-197912_t1.rds",
  "climate/out_files/brazil_prlr_198001-200912_t1.rds",
  "climate/out_files/brazil_prlr_201001-202512_t1.rds"
)

tas_files <- c(
  "climate/out_files/brazil_tas_195001-197912_t1.rds",
  "climate/out_files/brazil_tas_198001-200912_t1.rds",
  "climate/out_files/brazil_tas_201001-202512_t1.rds"
)

prlr_raster_files <- purrr::map_chr(prlr_files, convert_chunk_to_raster)
tas_raster_files <- purrr::map_chr(tas_files, convert_chunk_to_raster)

# Extract --------------------------------------------------------------------
# prlr_raster <- readRDS(paste0("climate/out_files/brazil_prlr_", period, "_t2.rds"))
# tas_raster <- readRDS(paste0("climate/out_files/brazil_tas_", period, "_t2.rds"))
# 
# adm <- readRDS("resources/shp/hr_shapefile.rds")
# sf_use_s2(FALSE)  
# adm_centroid <- st_transform(sf::st_centroid(adm), crs = 4326)
# sf_use_s2(TRUE)  
# adm <- st_transform(adm, crs = 4326)
# adm_id <- adm$regional_geocode
# adm_name <- adm$regional_name
# adm_latitude <- unname(sf::st_coordinates(adm_centroid)[, "Y"])
# 
# var <- c("prlr", "tas")
# 
# var1_raster <- get(paste0(var[1], "_raster"))
# ntimestep <- raster::nlayers(var1_raster)
# monthly_adm <- data.frame()
# for (l in 1:ntimestep){
#   name <- names(var1_raster[[l]])
#   name <- substr(name, 2, 11)
# 
#   # create dataframe for spatial and temporal ids
#   df1 <- data.frame(month = rep(substr(name, 6, 7), times = length(adm_id)),
#                     year = rep(substr(name, 1, 4), times = length(adm_id)),
#                     adm_id = adm_id,
#                     adm_name = adm_name,
#                     adm_latitude = adm_latitude)
# 
#   # loop over all the variables
#   for (v in var){
#     var_raster <- get(paste0(v, "_raster"))
#     df1[[v]] <- exactextractr::exact_extract(var_raster[[l]], adm, "mean", progress=FALSE)
#   }
#   monthly_adm <- rbind(monthly_adm, df1)
#   print(l)
# }
# 
# # save data
# write.csv(monthly_adm,
#           paste0("climate/out_files/brazil_healthregions_tas_prlr_", period, ".csv"),
#           row.names = FALSE)
# rm("monthly_adm")

prlr_raster_files <- c(
  "climate/out_files/brazil_prlr_195001-197912_t2.rds",
  "climate/out_files/brazil_prlr_198001-200912_t2.rds",
  "climate/out_files/brazil_prlr_201001-202512_t2.rds"
)

tas_raster_files <- c(
  "climate/out_files/brazil_tas_195001-197912_t2.rds",
  "climate/out_files/brazil_tas_198001-200912_t2.rds",
  "climate/out_files/brazil_tas_201001-202512_t2.rds"
)

adm <- readRDS("resources/health_region/shp/hr_shapefile.rds")

sf::sf_use_s2(FALSE)
adm_centroid <- sf::st_transform(sf::st_centroid(adm), crs = 4326)
sf::sf_use_s2(TRUE)

adm <- sf::st_transform(adm, crs = 4326)

adm_id <- adm$regional_geocode
adm_name <- adm$regional_name
adm_latitude <- unname(sf::st_coordinates(adm_centroid)[, "Y"])

monthly_chunk_files <- purrr::map2_chr(
  prlr_raster_files,
  tas_raster_files,
  extract_chunk
)

monthly_adm <- monthly_chunk_files |>
  purrr::map(read.csv) |>
  dplyr::bind_rows() |>
  dplyr::arrange(adm_id, year, month)

write.csv(
  monthly_adm,
  paste0("climate/out_files/brazil_healthregions_tas_prlr_", period, ".csv"),
  row.names = FALSE
)

rm(monthly_adm)
gc()

# Temp rolling means--------------------------------------------------------

# monthly_adm <- read.csv(paste0("climate/out_files/brazil_healthregions_tas_prlr_", period, ".csv"))
# monthly_adm <- monthly_adm[order(monthly_adm$adm_id, monthly_adm$year, monthly_adm$month), ]
# 
# # initialize the columns for accumulated spi/spei
# monthly_adm$tas3 <- NA
# monthly_adm$tas6 <- NA
# monthly_adm$tas12 <- NA
# 
# # Get the unique adm_ids
# adm_ids <- unique(monthly_adm$adm_id)
# # Loop over each adm_id to calculate the rolling average
# 
# for (adm_id in adm_ids) {
#   # Get the indices of the current adm_id
#   idx <- which(monthly_adm$adm_id == adm_id)
# 
#   # Calculate the rolling sum for the current adm_id
#   for (i in 3:length(idx)) {
#     monthly_adm$tas3[idx[i]] <- mean(monthly_adm$tas[idx[(i - 2):i]], na.rm = TRUE)
#   }
# 
#   # Calculate the 6-month rolling sum for the current adm_id
#   for (i in 6:length(idx)) {
#     monthly_adm$tas6[idx[i]] <- mean(monthly_adm$tas[idx[(i - 5):i]], na.rm = TRUE)
#   }
# 
#   # Calculate the 12-month rolling sum for the current adm_id
#   for (i in 12:length(idx)) {
#     monthly_adm$tas12[idx[i]] <- mean(monthly_adm$tas[idx[(i - 11):i]], na.rm = TRUE)
#   }
#   print(paste0(match(adm_id, adm_ids), " from ", length(adm_ids)))
# }
# 
# # save data
# write.csv(monthly_adm,
#           paste0("climate/out_files/brazil_healthregions_tas_prlr_", period, "_roll.csv"),
#           row.names = FALSE)
# rm("monthly_adm")

monthly_adm <- read.csv(
  paste0("climate/out_files/brazil_healthregions_tas_prlr_", period, ".csv")
)

monthly_adm <- monthly_adm |>
  dplyr::arrange(adm_id, year, month) |>
  dplyr::group_by(adm_id) |>
  dplyr::mutate(
    tas3 = zoo::rollmean(tas, k = 3, fill = NA, align = "right"),
    tas6 = zoo::rollmean(tas, k = 6, fill = NA, align = "right"),
    tas12 = zoo::rollmean(tas, k = 12, fill = NA, align = "right")
  ) |>
  dplyr::ungroup()

write.csv(
  monthly_adm,
  paste0("climate/out_files/brazil_healthregions_tas_prlr_", period, "_roll.csv"),
  row.names = FALSE
)

rm(monthly_adm)
gc()


# Anomalies, SPI and SPEI ------------------------------------------------------
# monthly_adm <- read.csv(paste0("climate/out_files/brazil_healthregions_tas_prlr_", period, "_roll.csv"))
# monthly_adm <- monthly_adm[order(monthly_adm$adm_id, monthly_adm$year, monthly_adm$month), ]

# # Reference period (1981-2010)
# ref_year1 <- 1981 # For temp
# ref_year2 <- 2010 # For temp, SPI, SPEI

# # Parameters lists
# p_tasan1 <- list()
# p_tasan3 <- list()
# p_tasan6 <- list()
# p_tasan12 <- list()
# p_spi1 <- list()
# p_spi3 <- list()
# p_spi6 <- list()
# p_spi12 <- list()
# p_spei1 <- list()
# p_spei3 <- list()
# p_spei6 <- list()
# p_spei12 <- list()

# # Computations
# source("climate/utils.R")

# all_adm <- data.frame()
# for(adm_id in unique(monthly_adm$adm_id)){

#   # Subset of data
#   adm <- monthly_adm[monthly_adm$adm_id == adm_id,]
  
#   # TS
#   tas1_ts <- ts(adm$tas, start = c(1950, 1), frequency = 12)
#   tas3_ts <- ts(adm$tas3, start = c(1950, 1), frequency = 12)
#   tas6_ts <- ts(adm$tas6, start = c(1950, 1), frequency = 12)
#   tas12_ts <- ts(adm$tas12, start = c(1950, 1), frequency = 12)
#   prlr_ts <- ts(adm$prlr, start = c(1950, 1), frequency = 12)
#   adm$pet <- thornthwaite(adm$tas, adm$adm_latitude[1], verbose = FALSE)
#   adm$plrl_pet <- adm$prlr - adm$pet
#   prlr2_ts <- ts(adm$plrl_pet, start = c(1950, 1), frequency = 12)
  
#   # Temp anomalies
#   tasan1_adm <- monthly_temp_anomaly(tas1_ts, ref.start = c(ref_year1, 1), ref.end = c(ref_year2, 12))
#   tasan3_adm <- monthly_temp_anomaly(tas3_ts, ref.start = c(ref_year1, 1), ref.end = c(ref_year2, 12))
#   tasan6_adm <- monthly_temp_anomaly(tas6_ts, ref.start = c(ref_year1, 1), ref.end = c(ref_year2, 12))
#   tasan12_adm <- monthly_temp_anomaly(tas12_ts, ref.start = c(ref_year1, 1), ref.end = c(ref_year2, 12))
  
#   # SPI
#   spi1_adm <- spi(prlr_ts, scale = 1, ref.end = c(ref_year2, 12), verbose = FALSE)
#   spi3_adm <- spi(prlr_ts, scale = 3, ref.end = c(ref_year2, 12), verbose = FALSE)
#   spi6_adm <- spi(prlr_ts, scale = 6, ref.end = c(ref_year2, 12), verbose = FALSE)
#   spi12_adm <- spi(prlr_ts, scale = 12, ref.end = c(ref_year2, 12), verbose = FALSE)

#   # SPEI
#   spei1_adm <- spei(prlr2_ts, scale = 1, ref.end = c(ref_year2, 12), verbose = FALSE)
#   spei3_adm <- spei(prlr2_ts, scale = 3, ref.end = c(ref_year2, 12), verbose = FALSE)
#   spei6_adm <- spei(prlr2_ts, scale = 6, ref.end = c(ref_year2, 12), verbose = FALSE)
#   spei12_adm <- spei(prlr2_ts, scale = 12, ref.end = c(ref_year2, 12), verbose = FALSE)

#   # Save params in lists with name of the list equal to the ID
#   p_tasan1[[as.character(adm_id)]] <- tasan1_adm$params
#   p_tasan3[[as.character(adm_id)]] <- tasan3_adm$params
#   p_tasan6[[as.character(adm_id)]] <- tasan6_adm$params
#   p_tasan12[[as.character(adm_id)]] <- tasan12_adm$params
#   p_spi1[[as.character(adm_id)]] <- coefficients(spi1_adm)
#   p_spi3[[as.character(adm_id)]] <- coefficients(spi3_adm)
#   p_spi6[[as.character(adm_id)]] <- coefficients(spi6_adm)
#   p_spi12[[as.character(adm_id)]] <- coefficients(spi12_adm)
#   p_spei1[[as.character(adm_id)]] <- coefficients(spei1_adm)
#   p_spei3[[as.character(adm_id)]] <- coefficients(spei3_adm)
#   p_spei6[[as.character(adm_id)]] <- coefficients(spei6_adm)
#   p_spei12[[as.character(adm_id)]] <- coefficients(spei12_adm)
  
#   # Parse and trim Inf values
#   trim_vals <- function(x){
#     v <- c(x)
#     ifelse(v==Inf, 6, ifelse(v==-Inf, -6, v))
#   }
  
#   # Write params in df
#   adm$tasan1 <- trim_vals(tasan1_adm$anomaly)
#   adm$tasan3 <- trim_vals(tasan3_adm$anomaly)
#   adm$tasan6 <- trim_vals(tasan6_adm$anomaly)
#   adm$tasan12 <- trim_vals(tasan12_adm$anomaly)
#   adm$spi1 <- trim_vals(fitted(spi1_adm))
#   adm$spi3 <- trim_vals(fitted(spi3_adm))
#   adm$spi6 <- trim_vals(fitted(spi6_adm))
#   adm$spi12 <- trim_vals(fitted(spi12_adm))
#   adm$spei1 <- trim_vals(fitted(spei1_adm))
#   adm$spei3 <- trim_vals(fitted(spei3_adm))
#   adm$spei6 <- trim_vals(fitted(spei6_adm))
#   adm$spei12 <- trim_vals(fitted(spei12_adm))
  
#   # progress
#   all_adm <- rbind(all_adm, adm)
#   print(paste0(match(adm_id, unique(monthly_adm$adm_id)),
#                "/", length(unique(monthly_adm$adm_id))))
# }

# # Keep relevant columns
# all_adm <- all_adm[c("month", "year", "adm_id",
#                      "tasan1", "tasan3", "tasan6", "tasan12", "spi1", "spi3",    
#                      "spi6", "spi12", "spei1", "spei3", "spei6", "spei12")]

# # save data
# write.csv(all_adm,
#           paste0("climate/out_files/brazil_monthly_hist_spi_", period, ".csv"),
#           row.names = FALSE)

# # save parameter lists
# saveRDS(p_tasan1, paste0("climate/out_files/tasan1_params.rds"))
# saveRDS(p_tasan3, paste0("climate/out_files/tasan3_params.rds"))
# saveRDS(p_tasan6, paste0("climate/out_files/tasan6_params.rds"))
# saveRDS(p_tasan12, paste0("climate/out_files/tasan12_params.rds"))
# saveRDS(p_spi1, paste0("climate/out_files/spi1_params.rds"))
# saveRDS(p_spi3, paste0("climate/out_files/spi3_params.rds"))
# saveRDS(p_spi6, paste0("climate/out_files/spi6_params.rds"))
# saveRDS(p_spi12, paste0("climate/out_files/spi12_params.rds"))
# saveRDS(p_spei1, paste0("climate/out_files/spei1_params.rds"))
# saveRDS(p_spei3, paste0("climate/out_files/spei3_params.rds"))
# saveRDS(p_spei6, paste0("climate/out_files/spei6_params.rds"))
# saveRDS(p_spei12, paste0("climate/out_files/spei12_params.rds"))

monthly_adm <- read.csv(
  paste0("climate/out_files/brazil_healthregions_tas_prlr_", period, "_roll.csv")
) |>
  dplyr::arrange(adm_id, year, month)

ref_year1 <- 1981
ref_year2 <- 2010

source("climate/utils.R")

trim_vals <- function(x) {
  v <- c(x)
  dplyr::case_when(
    is.infinite(v) & v > 0 ~ 6,
    is.infinite(v) & v < 0 ~ -6,
    TRUE ~ v
  )
}

compute_adm_indices <- function(adm, adm_id, i, n) {
  
  cat(i, "/", n, " - ", adm_id, "\n", sep = "")
  
  tas1_ts <- ts(adm$tas, start = c(1950, 1), frequency = 12)
  tas3_ts <- ts(adm$tas3, start = c(1950, 1), frequency = 12)
  tas6_ts <- ts(adm$tas6, start = c(1950, 1), frequency = 12)
  tas12_ts <- ts(adm$tas12, start = c(1950, 1), frequency = 12)
  prlr_ts <- ts(adm$prlr, start = c(1950, 1), frequency = 12)
  
  pet <- SPEI::thornthwaite(
    adm$tas,
    adm$adm_latitude[1],
    verbose = FALSE
  )
  
  prlr_pet_ts <- ts(adm$prlr - pet, start = c(1950, 1), frequency = 12)
  
  tasan1 <- monthly_temp_anomaly(
    tas1_ts,
    ref.start = c(ref_year1, 1),
    ref.end = c(ref_year2, 12)
  )
  tasan3 <- monthly_temp_anomaly(
    tas3_ts,
    ref.start = c(ref_year1, 1),
    ref.end = c(ref_year2, 12)
  )
  tasan6 <- monthly_temp_anomaly(
    tas6_ts,
    ref.start = c(ref_year1, 1),
    ref.end = c(ref_year2, 12)
  )
  tasan12 <- monthly_temp_anomaly(
    tas12_ts,
    ref.start = c(ref_year1, 1),
    ref.end = c(ref_year2, 12)
  )
  
  spi1 <- SPEI::spi(prlr_ts, scale = 1, ref.end = c(ref_year2, 12), verbose = FALSE)
  spi3 <- SPEI::spi(prlr_ts, scale = 3, ref.end = c(ref_year2, 12), verbose = FALSE)
  spi6 <- SPEI::spi(prlr_ts, scale = 6, ref.end = c(ref_year2, 12), verbose = FALSE)
  spi12 <- SPEI::spi(prlr_ts, scale = 12, ref.end = c(ref_year2, 12), verbose = FALSE)
  
  spei1 <- SPEI::spei(prlr_pet_ts, scale = 1, ref.end = c(ref_year2, 12), verbose = FALSE)
  spei3 <- SPEI::spei(prlr_pet_ts, scale = 3, ref.end = c(ref_year2, 12), verbose = FALSE)
  spei6 <- SPEI::spei(prlr_pet_ts, scale = 6, ref.end = c(ref_year2, 12), verbose = FALSE)
  spei12 <- SPEI::spei(prlr_pet_ts, scale = 12, ref.end = c(ref_year2, 12), verbose = FALSE)
  
  out <- adm |>
    dplyr::transmute(
      month,
      year,
      adm_id,
      tasan1 = trim_vals(tasan1$anomaly),
      tasan3 = trim_vals(tasan3$anomaly),
      tasan6 = trim_vals(tasan6$anomaly),
      tasan12 = trim_vals(tasan12$anomaly),
      spi1 = trim_vals(fitted(spi1)),
      spi3 = trim_vals(fitted(spi3)),
      spi6 = trim_vals(fitted(spi6)),
      spi12 = trim_vals(fitted(spi12)),
      spei1 = trim_vals(fitted(spei1)),
      spei3 = trim_vals(fitted(spei3)),
      spei6 = trim_vals(fitted(spei6)),
      spei12 = trim_vals(fitted(spei12))
    )
  
  params <- list(
    tasan1 = tasan1$params,
    tasan3 = tasan3$params,
    tasan6 = tasan6$params,
    tasan12 = tasan12$params,
    spi1 = coefficients(spi1),
    spi3 = coefficients(spi3),
    spi6 = coefficients(spi6),
    spi12 = coefficients(spi12),
    spei1 = coefficients(spei1),
    spei3 = coefficients(spei3),
    spei6 = coefficients(spei6),
    spei12 = coefficients(spei12)
  )
  
  list(data = out, params = params)
}

adm_ids <- unique(monthly_adm$adm_id)

results <- purrr::map2(
  adm_ids,
  seq_along(adm_ids),
  \(adm_id, i) {
    adm <- monthly_adm |>
      dplyr::filter(.data$adm_id == !!adm_id)
    
    compute_adm_indices(
      adm = adm,
      adm_id = adm_id,
      i = i,
      n = length(adm_ids)
    )
  }
)

all_adm <- results |>
  purrr::map("data") |>
  dplyr::bind_rows()

params <- results |>
  purrr::map("params")

names(params) <- as.character(adm_ids)

write.csv(
  all_adm,
  paste0("climate/out_files/brazil_monthly_hist_spi_", period, ".csv"),
  row.names = FALSE
)

save_named_params <- function(param_name) {
  param_list <- purrr::map(params, param_name)
  
  saveRDS(
    param_list,
    paste0("climate/out_files/", param_name, "_params.rds")
  )
}

purrr::walk(
  c(
    "tasan1", "tasan3", "tasan6", "tasan12",
    "spi1", "spi3", "spi6", "spi12",
    "spei1", "spei3", "spei6", "spei12"
  ),
  save_named_params
)

rm(monthly_adm, results, params, all_adm)
gc()

### END
