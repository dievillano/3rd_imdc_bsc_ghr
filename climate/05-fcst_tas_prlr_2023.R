# Script name: 05_fcst_tas_prlr.R
#
# Obtain cliamte forecasts for the sprint
# 
# Author: Carles Milà (adapted from Daniela Lührsen's script)
# Date Created: 2025-07-28
# Email: carles.milagarcia@bsc.es
#
# Env: HUB with module R-bundle-CRAN/2024.06-foss-2023b and CDO/2.4.4-gompi-2023b

# libraries
library(lubridate)
library(startR) # make sure to load module CDO
library(CSIndicators)
library(CSTools)
library(ncdf4)
library(sf)

# select boundary box (Brazil):
lats.min <- -35
lats.max <- 15
lons.min <- -80
lons.max <- -30

# select dates:
mm <- 6
syear <- 2023
calibration.period <- 1993:2016
nleadtimes <- 7

# define functions to load data:

# load reanalysis (ERA5-Land), retrieve FALSE (to use compute)
load.era5land <- function(var, mm, calibration.period, nleadtimes, retrieve = TRUE) {
  # config parameters for the path in esarchive:
  if (var == "tas" | var == "tdps" | var == "prlr") { freq.re <- "f1h" }
  if (var == "tasmax" | var == "tasmin") { freq.re <- "f24h" }

  # prepare dates format:
  sdate.base <- paste0(calibration.period, ifelse(mm < 10, "0", ""), mm)
  sdate <- sdate.base
  for (n in 2:nleadtimes){
    sdate <- cbind(sdate, substr(gsub("-","",as.character(ymd(as.Date(paste0(substr(sdate.base,1,4), "-", substr(sdate.base,5,6), "-01"))) %m+% months(n-1))),1,6))
  }
  names(dim(sdate)) <- c("sdate", "time")

  # load data:
  ref <- startR::Start(dat = "/esarchive/recon/ecmwf/era5land/monthly_mean/$var$_$freq$/$var$_$sdate$.nc",
                       var = var,
                       freq = freq.re,
                       sdate = sdate,
                       split_multiselected_dims = TRUE,
                       latitude = startR::values(list(lats.min, lats.max)),
                       latitude_reorder = startR::Sort(decreasing = TRUE),
                       longitude = startR::values(list(lons.min, lons.max)),
                       longitude_reorder = startR::CircularSort(-180, 180),
                       synonims = list(latitude = c("lat", "latitude"),
                                       longitude = c("lon", "longitude")),
                       return_vars = list(latitude = "dat",
                                          longitude = "dat",
                                          time = c("sdate")),
                       retrieve = retrieve)
  return(ref)
}

# load reanalysis (ERA5), retrieve FALSE (to use compute)
load.era5 <- function(var, mm, calibration.period, nleadtimes, retrieve = TRUE) {
  # config parameters for the path in esarchive:
  if (var == "tas") {freq.re <- "f1h"}
  if (var == "tasmax" | var == "tasmin") {freq.re <- "f24h-r1440x721cds"}
  if (var == "tdps" | var == "prlr") {freq.re <- "f1h-r1440x721cds"}

  # prepare dates format:
  sdate.base <- paste0(calibration.period, ifelse(mm < 10, "0", ""), mm)
  sdate <- sdate.base
  for (n in 2:nleadtimes){
    sdate <- cbind(sdate, substr(gsub("-","",as.character(ymd(as.Date(paste0(substr(sdate.base,1,4), "-", substr(sdate.base,5,6), "-01"))) %m+% months(n-1))),1,6))
  }
  names(dim(sdate)) <- c("sdate", "time")

  # load data:
  ref <- startR::Start(dat = "/esarchive/recon/ecmwf/era5/monthly_mean/$var$_$freq$/$var$_$sdate$.nc",
                       var = var,
                       freq = freq.re,
                       sdate = sdate,
                       split_multiselected_dims = TRUE,
                       latitude = startR::values(list(lats.min, lats.max)),
                       latitude_reorder = startR::Sort(decreasing = TRUE),
                       longitude = startR::values(list(lons.min, lons.max)),
                       longitude_reorder = startR::CircularSort(-180, 180),
                       synonims = list(latitude = c("lat", "latitude"),
                                       longitude = c("lon", "longitude")),
                       return_vars = list(latitude = "dat",
                                           longitude = "dat",
                                           time = c("sdate")),
                       retrieve = retrieve)
  return(ref)
}

# load hindcast (SEAS-5.1), retrieve FALSE (to use compute)
load.hcst <- function(var, mm, calibration.period, nleadtimes, retrieve = TRUE, regrid_to = "era5"){
  # config parameters for the path in esarchive:
  if (var == "tas" | var == "tdps") {freq.se <- "f6h"}
  if (var == "tasmax" | var == "tasmin") {freq.se <- "f24h"}
  if (var == "prlr") {freq.se <- "s0-24h"}

  # regrid parameters to regrid hcst and fcst to the same spatial resolution as the reference
  if (var == "prlr") {
    regrid.method <- "con"
    if (regrid_to == "era5land"){regrid <- "/esarchive/recon/ecmwf/era5land/monthly_mean/tas_f1h/tas_202001.nc"} # sample file for the spatial resolution
    if (regrid_to == "era5"){regrid <- "/esarchive/recon/ecmwf/era5/monthly_mean/prlr_f1h-r1440x721cds/prlr_202001.nc"} # sample file for the spatial resolution
  } else {
    regrid.method <- "bil"
    if (regrid_to == "era5land"){regrid <- "/esarchive/recon/ecmwf/era5land/monthly_mean/tas_f1h/tas_202001.nc"} # sample file for the spatial resolution
    if (regrid_to == "era5"){regrid <- "/esarchive/recon/ecmwf/era5/monthly_mean/tas_f1h/tas_202001.nc"} # sample file for the spatial resolution
  }
  # prepare dates format:
  sdate.base <- paste0(calibration.period, ifelse(mm < 10, "0", ""), mm)

  # load data:
  hcst <- startR::Start(dat = "/esarchive/exp/ecmwf/system51c3s/monthly_mean/$var$_$freq$/$var$_$sdate$01.nc",
                        var = var,
                        freq = freq.se,
                        sdate = sdate.base,
                        time = 1:nleadtimes,
                        ensemble = 1:25,
                        split_multiselected_dims = TRUE,
                        transform = startR::CDORemapper,
                        transform_params = list(grid = regrid, method = regrid.method),
                        transform_vars = c("latitude", "longitude"),
                        latitude = startR::values(list(lats.min, lats.max)),
                        latitude_reorder = startR::Sort(decreasing = TRUE),
                        longitude = startR::values(list(lons.min, lons.max)),
                        longitude_reorder = startR::CircularSort(-180, 180),
                        synonims = list(latitude = c("lat", "latitude"),
                                        longitude = c("lon", "longitude"),
                                        ensemble = c("lev", "ensemble")),
                        return_vars = list(latitude = "dat",
                                           longitude = "dat",
                                           time = c("sdate")),
                        retrieve = retrieve)
  return(hcst)
}

# load forecast (SEAS-5.1), retrieve FALSE (to use compute)
load.fcst <- function(var, mm, syear, nleadtimes, retrieve = TRUE, regrid_to = "era5") {
  # config parameters for the path in esarchive:
  if (var == "tas" | var == "tdps") { freq.se <- "f6h" }
  if (var == "tasmax" | var == "tasmin" ) { freq.se <- "f24h" }
  if (var == "prlr") { freq.se <- "s0-24h" }

  # regrid parameters to regrid hcst and fcst to the same spatial resolution as the reference
  if (var == "prlr") {
    regrid.method <- "con"
    if (regrid_to == "era5land") {regrid <- "/esarchive/recon/ecmwf/era5land/monthly_mean/tas_f1h/tas_202001.nc"} # sample file for the spatial resolution
    if (regrid_to == "era5") {regrid <- "/esarchive/recon/ecmwf/era5/monthly_mean/prlr_f1h-r1440x721cds/prlr_202001.nc"} # sample file for the spatial resolution

  } else {
    regrid.method <- "bil"
    if (regrid_to == "era5land") {regrid <- "/esarchive/recon/ecmwf/era5land/monthly_mean/tas_f1h/tas_202001.nc"} # sample file for the spatial resolution
    if (regrid_to == "era5") {regrid <- "/esarchive/recon/ecmwf/era5/monthly_mean/tas_f1h/tas_202001.nc"} # sample file for the spatial resolution
  }
  # prepare dates format:
  sdate.fcst <- paste0(syear, ifelse(mm < 10, "0", ""), mm)

  # load data:
  fcst <- startR::Start(dat = "/esarchive/exp/ecmwf/system51c3s/monthly_mean/$var$_$freq$/$var$_$sdate$01.nc",
                        var = var,
                        freq = freq.se,
                        sdate = sdate.fcst,
                        time = 1:nleadtimes,
                        ensemble = 1:51,
                        split_multiselected_dims = TRUE,
                        transform = startR::CDORemapper,
                        transform_params = list(grid = regrid, method = regrid.method),
                        transform_vars = c("latitude", "longitude"),
                        latitude = startR::values(list(lats.min, lats.max)),
                        latitude_reorder = startR::Sort(decreasing = TRUE),
                        longitude = startR::values(list(lons.min, lons.max)),
                        longitude_reorder = startR::CircularSort(-180, 180),
                        synonims = list(latitude = c("lat", "latitude"),
                                        longitude = c("lon", "longitude"),
                                        ensemble = c("lev", "ensemble")),
                        return_vars = list(latitude = "dat",
                                           longitude = "dat",
                                           time = c("sdate")),
                        retrieve = retrieve)
  return(fcst)
}

# tas ----

var <- "tas"
ref <- load.era5(var, mm, calibration.period, nleadtimes, retrieve = FALSE)
hcst <- load.hcst(var, mm, calibration.period, nleadtimes, retrieve = FALSE, regrid_to = "era5")
fcst <- load.fcst(var, mm, syear, nleadtimes, retrieve = FALSE, regrid_to = "era5")

fcst_calibration_tas <- function(ref, hcst, fcst) {
  result <- CSTools::Calibration(exp = hcst, obs = ref, exp_cor = fcst,
                                 cal.method = "evmos", multi.model = FALSE,
                                 eval.method = "leave-one-out", na.fill = TRUE,
                                 na.rm = TRUE, apply_to = NULL, alpha = NULL,
                                 memb_dim = "ensemble", sdate_dim = "sdate",
                                 dat_dim = NULL, ncores = 7)
  # transform units
  result <- result - 273.15

  return(result)
}

save.image(file = paste0("temp_files/tas_test_",
                         syear, sprintf("%02d", mm), ".RData"))

step <- Step(fun = fcst_calibration_tas,
             target_dims = list(ref = c("sdate", "time"),
                                hcst = c("sdate", "time", "ensemble"),
                                fcst = c("sdate", "time", "ensemble")),
                                output_dims = c("sdate", "time", "ensemble"))
wf <- AddStep(list(ref, hcst, fcst), step) # if any parameters for the function used in step, they should be added here
fcst.cal <- Compute(wf, chunks = list(longitude = 10, latitude = 10),
                    threads_load = 8, threads_compute = 8)$output1

save(fcst.cal, file = paste0("temp_files/brazil_fcst_cal_",
                             syear, sprintf("%02d", mm), "_tas.RData"))

fcst$latitude

# save fcst.cal as netcdf
xvals <- attr(fcst, "Variables")$dat1$longitude
yvals <- attr(fcst, "Variables")$dat1$latitude
lon1 <- ncdim_def("longitude", "degrees_east", xvals)
lat2 <- ncdim_def("latitude", "degrees_north", yvals)
leadtime <- ncdim_def("leadtime", "month", 1:nleadtimes)
ensemble <- ncdim_def("ensemble", "", 1:51)

var <- ncvar_def(name = "calibrated_forecast",
                 units = "C",
                 dim = list(lon1, lat2, leadtime, ensemble),
                 longname = "calibrated forecast of 2 meter temperature",
                 missval = -999)

ncnew <- nc_create(paste0("temp_files/calibrated_forecast_",
                          syear, sprintf("%02d", mm), "_tas.nc"), list(var))
ncvar_put(nc = ncnew,
          varid = "calibrated_forecast",
          vals = aperm(drop(fcst.cal), c(4, 3, 1, 2)))
nc_close(ncnew)

print("saved temperature calibrated forecast")
rm(ref, hcst, fcst, step, wf, fcst.cal)

# prlr ----

var <- "prlr"
ref <- load.era5(var, mm, calibration.period, nleadtimes, retrieve = FALSE)
hcst <- load.hcst(var, mm, calibration.period, nleadtimes, retrieve = FALSE, regrid_to = "era5")
fcst <- load.fcst(var, mm, syear, nleadtimes, retrieve = FALSE, regrid_to = "era5")

fcst_calibration_prlr <- function(ref, hcst, fcst) {
  result <- CSTools::QuantileMapping(exp = hcst, obs = ref, exp_cor = fcst,
                                     sdate_dim = "sdate", memb_dim = "ensemble",
                                     method = "QUANT", na.rm = TRUE, ncores = 7)
  # transform units
  result <- result * 3600 * 24 * 30.44 * 1000

  return(result)
}

save.image(file = paste0("temp_files/prlr_test_",
                        syear, sprintf("%02d", mm), ".RData"))

step <- Step(fun = fcst_calibration_prlr,
             target_dims = list(ref = c("sdate", "time"),
                                hcst = c("sdate", "time", "ensemble"),
                                fcst = c("sdate", "time", "ensemble")),
             output_dims = c("sdate", "time", "ensemble"))

wf <- AddStep(list(ref, hcst, fcst), step) # if any parameters for the function used in step, they should be added here

fcst.cal <- Compute(wf, chunks = list(longitude = 10, latitude = 10),
                    threads_load = 8, threads_compute = 8)$output1

save(fcst.cal, file = paste0("temp_files/brazil_fcst_cal_",
                             syear, sprintf("%02d", mm), "_prlr.RData"))


# save fcst.cal as netcdf
xvals <- attr(fcst, "Variables")$dat1$longitude
yvals <- attr(fcst, "Variables")$dat1$latitude
lon1 <- ncdim_def("longitude", "degrees_east", xvals)
lat2 <- ncdim_def("latitude", "degrees_north", yvals)
leadtime <- ncdim_def("leadtime", "month", 1:nleadtimes)
ensemble <- ncdim_def("ensemble", "", 1:51)

var <- ncvar_def(name = "calibrated_forecast",
                 units = "mm",
                 dim = list(lon1, lat2, leadtime, ensemble),
                 longname = "calibrated forecast of total precipitation",
                 missval = -999)

ncnew <- nc_create(paste0("temp_files/calibrated_forecast_",
                          syear, sprintf("%02d", mm), "_prlr.nc"), list(var))
ncvar_put(nc = ncnew,
          varid = "calibrated_forecast",
          vals = aperm(drop(fcst.cal), c(4, 3, 1, 2))) # potential issues if the result of fcst_calibration_prlr has the dims in different order
nc_close(ncnew)

print("saved precipitation calibrated forecast")
rm(ref, hcst, fcst, step, wf, fcst.cal)


# Zonal statistics--------------------------------

shape <- sf::st_read("boundaries/shape_regional_health.gpkg") |> 
  st_transform(crs = 4326)
nc_files <- list(tas = paste0("temp_files/calibrated_forecast_", syear, sprintf("%02d", mm), "_tas.nc"),
                 prlr = paste0("temp_files/calibrated_forecast_", syear, sprintf("%02d", mm), "_prlr.nc"))

for (file in names(nc_files)) {
  # get data from netcdf file
  nc <- nc_open(nc_files[[file]])
  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  leadtime <- ncvar_get(nc, "leadtime")
  ensemble <- ncvar_get(nc, "ensemble")
  data <- ncvar_get(nc, "calibrated_forecast")
  nc_close(nc)

  result <- data.frame()

  for (l in seq_along(leadtime)) {
    for (e in seq_along(ensemble)) {
      r <- raster::raster(t(data[, , l, e]), xmn = min(lon), xmx = max(lon),
                          ymn = min(lat), ymx = max(lat))
      raster::crs(r) <- "EPSG:4326"
      values <- exactextractr::exact_extract(r,
                                             shape,
                                             "mean",
                                             append_cols = "regional_geocode")
      values$leadtime <- l
      values$ensemble <- e
      result <- rbind(result, values)
      print(paste0("leadtime: ", l, " ensemble nr: ", e))
    }
  }

  # save output
  write.csv(result,
            paste0("out_files/brazil_monthly_fcst_", syear, sprintf("%02d", mm), "_", file, ".csv"),
            row.names = FALSE)
}
