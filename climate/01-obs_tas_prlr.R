# Script name: 01_obs_tas_prlr
#
# Calculate all temperature and precipitation for the sprint challenge on 
# a health region level.
# 
# Author: Carles Milà (adapted from Daniela Lührsen's script)
# Date Created: 2025-06-02
# Email: carles.milagarcia@bsc.es
#
# Env: HUB with module R-bundle-CRAN/2024.06-foss-2023b

resources_path  <- fs::path("resources/health_region")
shp_path  <- fs::path(resources_path, "shp")

packages <- c("startR", "s2dv", "CSTools", "multiApply", "ClimProjDiags", 
              "SPEI", "zoo", "TLMoments", "lmomco", "lmom", "lubridate",
              "sf")
lapply(packages, require, character.only = TRUE)
options(bitmapType = "cairo")

# Load local packages
library("exactextractr", lib = "/home/Earth/cmilagar/packages")

# select parameters:
dataset <- "era5land" # options "era5", "era5land", "chirps"
start_year <- 2008
end_year <- 2025
start_month <- 1
end_month <- 12

lats_min <- -35
lats_max <- 15
lons_min <- -80
lons_max <- -30

adm <- readRDS(fs::path(shp_path, "hr_shapefile.rds"))
adm <- st_transform(adm, crs = 4326)

# obtain dates for startR call:
for (mm in start_month:end_month){
  if (mm == start_month) {
    dates <- as.Date(paste0(start_year:end_year, "-", start_month, "-1"))
  } else {
    dates <- append(dates, as.Date(paste0(start_year:end_year, "-", mm, "-1")))
  }
}
dates <- dates[order(dates)]
dates <- array(substr(gsub("-", "", as.character(dates)), 1, 6),
               dim = c(time = (end_month - start_month + 1),
                       syear = (end_year - start_year + 1)))

period <- paste0(start_year, sprintf("%02d", start_month), "-",
                 end_year, sprintf("%02d", end_month))

# precipitation ----

# obtain path for startR call:
path_dataset <- "/esarchive/recon/ecmwf/era5land/monthly_mean/$var$_f1h/$var$_$sdate$.nc"

# startR call:
data_prlr <- startR::Start(dat = path_dataset,
                           var = "prlr",
                           sdate = dates,
                           split_multiselected_dims = TRUE,
                           latitude = startR::values(list(lats_min, lats_max)),
                           latitude_reorder = startR::Sort(decreasing = TRUE),
                           longitude = startR::values(list(lons_min, lons_max)),
                           longitude_reorder = startR::CircularSort(-180, 180),
                           synonims = list(latitude = c("lat", "latitude"),
                                           longitude = c("lon", "longitude")),
                           return_vars = list(latitude = "dat",
                                              longitude = "dat",
                                              time = c("sdate")),
                           retrieve = TRUE)
data_prlr <- data_prlr * 3600 * 24 * 30.44 * 1000
attr(data_prlr, "Variables")$common$prlr$units <- "mm"
data_prlr <- CSTools::as.s2dv_cube(data_prlr)
if (!("ensemble" %in% names(data_prlr$data))) {
  data_prlr$data <- s2dv::InsertDim(data_prlr$data,
                                    pos =  5,
                                    len = 1,
                                    name = "ensemble")
}

fs::dir_create("climate/out_files")
saveRDS(data_prlr, paste0("climate/out_files/brazil_prlr_", period, ".rds"))
rm(data_prlr)

# tasmax ----

path_dataset <- "/esarchive/recon/ecmwf/era5land/monthly_mean/$var$_f24h/$var$_$sdate$.nc"

# startR call:
data_tasmax <- startR::Start(dat = path_dataset,
                             var = "tasmax",
                             sdate = dates,
                             split_multiselected_dims = TRUE,
                             latitude = startR::values(list(lats_min, lats_max)),
                             latitude_reorder = startR::Sort(decreasing = TRUE),
                             longitude = startR::values(list(lons_min, lons_max)),
                             longitude_reorder = startR::CircularSort(-180, 180),
                             synonims = list(latitude = c("lat", "latitude"),
                                             longitude = c("lon", "longitude")),
                             return_vars = list(latitude = "dat",
                                                longitude = "dat",
                                                time = c("sdate")),
                             retrieve = TRUE)
data_tasmax <- data_tasmax - 273.15
attr(data_tasmax, "Variables")$common$tasmax$units <- "C"
data_tasmax <- CSTools::as.s2dv_cube(data_tasmax)
if (!("ensemble" %in% names(data_tasmax$data))) {
  data_tasmax$data <- s2dv::InsertDim(data_tasmax$data,
                                      pos =  5,
                                      len = 1,
                                      name = "ensemble")
}
saveRDS(data_tasmax, paste0("climate/out_files/brazil_tasmax_", period, ".rds"))
rm(data_tasmax)

# tasmin ----

path_dataset <- "/esarchive/recon/ecmwf/era5land/monthly_mean/$var$_f24h/$var$_$sdate$.nc"

# startR call:
data_tasmin <- startR::Start(dat = path_dataset,
                             var = "tasmin",
                             sdate = dates,
                             split_multiselected_dims = TRUE,
                             latitude = startR::values(list(lats_min, lats_max)),
                             latitude_reorder = startR::Sort(decreasing = TRUE),
                             longitude = startR::values(list(lons_min, lons_max)),
                             longitude_reorder = startR::CircularSort(-180, 180),
                             synonims = list(latitude = c("lat", "latitude"),
                                             longitude = c("lon", "longitude")),
                             return_vars = list(latitude = "dat",
                                                longitude = "dat",
                                                time = c("sdate")),
                             retrieve = TRUE)
data_tasmin <- data_tasmin - 273.15
attr(data_tasmin, "Variables")$common$tasmin$units <- "C"
data_tasmin <- CSTools::as.s2dv_cube(data_tasmin)
if (!("ensemble" %in% names(data_tasmin$data))) {
  data_tasmin$data <- s2dv::InsertDim(data_tasmin$data,
                                      pos =  5,
                                      len = 1,
                                      name = "ensemble")
}
saveRDS(data_tasmin, paste0("climate/out_files/brazil_tasmin_", period, ".rds"))
rm(data_tasmin)

# tas ----

path_dataset <- "/esarchive/recon/ecmwf/era5land/monthly_mean/$var$_f1h/$var$_$sdate$.nc"

# startR call:
data_tas <- startR::Start(dat = path_dataset,
                          var = "tas",
                          sdate = dates,
                          split_multiselected_dims = TRUE,
                          latitude = startR::values(list(lats_min, lats_max)),
                          latitude_reorder = startR::Sort(decreasing = TRUE),
                          longitude = startR::values(list(lons_min, lons_max)),
                          longitude_reorder = startR::CircularSort(-180, 180),
                          synonims = list(latitude = c("lat", "latitude"),
                                          longitude = c("lon", "longitude")),
                          return_vars = list(latitude = "dat",
                                             longitude = "dat",
                                             time = c("sdate")),
                          retrieve = TRUE)
data_tas <- data_tas - 273.15
attr(data_tas, "Variables")$common$tas$units <- "C"
data_tas <- CSTools::as.s2dv_cube(data_tas)
if (!("ensemble" %in% names(data_tas$data))) {
  data_tas$data <- s2dv::InsertDim(data_tas$data,
                                   pos =  5,
                                   len = 1,
                                   name = "ensemble")
}
saveRDS(data_tas, paste0("climate/out_files/brazil_tas_", period, ".rds"))
rm(data_tas)

# convert to raster ----

# load data
data_prlr <- readRDS(paste0("climate/out_files/brazil_prlr_", period, ".rds"))
data_tas <- readRDS(paste0("climate/out_files/brazil_tas_",  period, ".rds"))
data_tasmin <- readRDS(paste0("climate/out_files/brazil_tasmin_",  period, ".rds"))
data_tasmax <- readRDS(paste0("climate/out_files/brazil_tasmax_",  period, ".rds"))

datas <- list(data_prlr, data_tas, data_tasmin, data_tasmax)

for (data in datas){
  print(data$attrs$Variable$varName)

  # Assign latitude and longitude coordinates from data
  lon <- data$coords$longitude
  lat <- data$coords$latitude

  # Set time variables
  nmonths <- data$dims[["time"]]
  nyears <- data$dims[["syear"]]
  ntimes <- nyears * nmonths

  # Transform multidimensional array into list of rasters
  out <- NULL
  for (x in 1:ntimes){

    # Calculate month and year from x
    m <- ((x - 1) %% nmonths) + 1
    y <- (((x - 1) %/% nmonths)) %% nyears + 1

    # Extract date
    b <- substr(data$attrs$Dates[x], 1, 10)

    if (is.na(b)) {
      print(paste(x, b))
      next
    }
    # Extract array of values for year-month-day combination
    a <-  data$data[1, 1, m, y, 1, , ] #month, year, day

    # Convert to a raster
    out[[b]] <- raster::raster(a,
                               xmn = min(lon), xmx = max(lon),
                               ymn = min(lat), ymx = max(lat))
    # Assign CRS
    raster::crs(out[[b]]) <- "EPSG:4326"
  }

  # Convert list of rasters into stack
  assign(paste0(data$attrs$Variable$varName, "_raster"), NULL)
  assign(paste0(data$attrs$Variable$varName, "_raster"), raster::stack(out))
  print(data$attrs$Variable$varName)
}


# Regional averages -------------------------

# load health region shapefile
adm_id <- adm$regional_geocode
adm_name <- adm$regional_name

var <- c("prlr", "tas", "tasmin", "tasmax")

var1_raster <- get(paste0(var[1], "_raster"))
ntimestep <- raster::nlayers(var1_raster)
monthly_adm <- data.frame()
for (l in 1:ntimestep){
  name <- names(var1_raster[[l]])
  name <- substr(name, 2, 11)

  # create dataframe for spatial and temporal ids
  df1 <- data.frame(month = rep(substr(name, 6, 7), times = length(adm_id)),
                    year = rep(substr(name, 1, 4), times = length(adm_id)),
                    adm_id = adm_id,
                    adm_name = adm_name)

  # loop over all the variables
  for (v in var){
    var_raster <- get(paste0(v, "_raster"))
    df1[[v]] <- exactextractr::exact_extract(var_raster[[l]], adm, "mean", progress = FALSE)
  }
  monthly_adm <- rbind(monthly_adm, df1)
  print(l)
}

# get actual timeperiod within data to name the file correctly
imonth <- min(monthly_adm$month[monthly_adm$year == min(monthly_adm$year)])
fmonth <- max(monthly_adm$month[monthly_adm$year == max(monthly_adm$year)])
period <- paste0(min(monthly_adm$year), imonth, "-", max(monthly_adm$year), fmonth)

# save
fs::dir_create("climate/out_files")
write.csv(monthly_adm,
          paste0("climate/out_files/brazil_monthly_hist_tas_prlr_",  period, ".csv"),
          row.names = FALSE)
