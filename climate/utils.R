monthly_temp_anomaly <- function(ts_data, ref.start = NULL, ref.end = NULL, params = NULL) {
  
  # Check if input is a ts object
  if (!inherits(ts_data, "ts")) {
    stop("ts_data must be a 'ts' object with monthly frequency.")
  }
  if (frequency(ts_data) != 12) {
    stop("ts_data must have monthly frequency.")
  }
  
  # Extract time indices
  time_index <- as.yearmon(time(ts_data))
  months <- cycle(ts_data)
  years <- floor(time(ts_data))

  # Create a data.frame
  df <- data.frame(
    value = as.numeric(ts_data),
    month = months,
    year = years,
    time = time_index
  )

  # Subset reference period if specified
  if (!is.null(ref.start) && !is.null(ref.end)) {
    ref_start <- as.yearmon(paste(ref.start, collapse = "-"))
    ref_end <- as.yearmon(paste(ref.end, collapse = "-"))
    df_ref <- df[df$time >= ref_start & df$time <= ref_end, ]
  } else {
    df_ref <- df
  }

  # Estimate parameters if not provided
  if (is.null(params)) {
    mu <- tapply(df_ref$value, df_ref$month, mean)
    sigma <- tapply(df_ref$value, df_ref$month, sd)
    params <- cbind(mu, sigma)
    colnames(params) <- c("mu", "sigma")
  }

  # Check params shape
  if (!is.matrix(params) || dim(params)[1] != 12 || dim(params)[2] != 2) {
    stop("params must be a 12 × 2 matrix with columns 'mu' and 'sigma'.")
  }

  # Compute anomalies
  df$mu <- params[df$month, 1]
  df$sigma <- params[df$month, 2]
  df$anomaly <- (df$value - df$mu) / df$sigma

  # Output list
  list(
    anomaly = ts(df$anomaly, start = start(ts_data), frequency = 12),
    params = params
  )
}

make_dates_chunk <- function(start_year, end_year) {
  dates <- expand.grid(
    month = start_month:end_month,
    year = start_year:end_year
  ) |>
    dplyr::arrange(year, month) |>
    dplyr::mutate(sdate = paste0(year, sprintf("%02d", month))) |>
    dplyr::pull(sdate)
  
  array(
    dates,
    dim = c(
      time = end_month - start_month + 1,
      syear = end_year - start_year + 1
    )
  )
}

get_tas_chunk <- function(start_year, end_year) {
  
  dates_chunk <- make_dates_chunk(start_year, end_year)
    
  period_chunk <- paste0(
    start_year, sprintf("%02d", start_month), "-",
    end_year, sprintf("%02d", end_month)
  )
  
  out_file <- paste0("temp_files/brazil_tas_", period_chunk, "_t1.rds")
  
  if (file.exists(out_file)) {
    cat("Already exists: ", out_file, "\n", sep = "")
    return(invisible(out_file))
  }
  
  data_tas <- startR::Start(
    dat = path_dataset,
    var = "tas",
    sdate = dates_chunk,
    split_multiselected_dims = TRUE,
    latitude = startR::values(list(lats_min, lats_max)),
    latitude_reorder = startR::Sort(decreasing = TRUE),
    longitude = startR::values(list(lons_min, lons_max)),
    longitude_reorder = startR::CircularSort(-180, 180),
    synonims = list(
      latitude = c("lat", "latitude"),
      longitude = c("lon", "longitude")
    ),
    return_vars = list(
      latitude = "dat",
      longitude = "dat",
      time = c("sdate")
    ),
    retrieve = TRUE
  )
  
  data_tas <- data_tas * 3600 * 24 * 30.44 * 1000
  attr(data_tas, "Variables")$common$tas$units <- "mm"
  data_tas <- CSTools::as.s2dv_cube(data_tas)
  
  if (!("ensemble" %in% names(data_tas$data))) {
    data_tas$data <- s2dv::InsertDim(
      data_tas$data,
      pos = 5,
      len = 1,
      name = "ensemble"
    )
  }
  
  saveRDS(data_tas, out_file)
  
  rm(data_tas)
  gc()
  
  invisible(out_file)
}

get_tas_chunk <- function(start_year, end_year) {
  
  dates_chunk <- make_dates_chunk(start_year, end_year)
    
  period_chunk <- paste0(
    start_year, sprintf("%02d", start_month), "-",
    end_year, sprintf("%02d", end_month)
  )
  
  out_file <- paste0("temp_files/brazil_tas_", period_chunk, "_t1.rds")
  
  if (file.exists(out_file)) {
    cat("Already exists: ", out_file, "\n", sep = "")
    return(invisible(out_file))
  }
  
  data_tas <- startR::Start(
    dat = path_dataset,
    var = "tas",
    sdate = dates_chunk,
    split_multiselected_dims = TRUE,
    latitude = startR::values(list(lats_min, lats_max)),
    latitude_reorder = startR::Sort(decreasing = TRUE),
    longitude = startR::values(list(lons_min, lons_max)),
    longitude_reorder = startR::CircularSort(-180, 180),
    synonims = list(
      latitude = c("lat", "latitude"),
      longitude = c("lon", "longitude")
    ),
    return_vars = list(
      latitude = "dat",
      longitude = "dat",
      time = c("sdate")
    ),
    retrieve = TRUE
  )
  
  data_tas <- data_tas - 273.15
  attr(data_tas, "Variables")$common$tas$units <- "C"
  data_tas <- CSTools::as.s2dv_cube(data_tas)
  
  if (!("ensemble" %in% names(data_tas$data))) {
    data_tas$data <- s2dv::InsertDim(
      data_tas$data,
      pos = 5,
      len = 1,
      name = "ensemble"
    )
  }
  
  saveRDS(data_tas, out_file)
  
  rm(data_tas)
  gc()
  
  invisible(out_file)
}

convert_chunk_to_raster <- function(file) {
  
  cat("Reading: ", file, "\n", sep = "")
  
  data <- readRDS(file)
  var_name <- data$attrs$Variable$varName
  
  cat("Converting variable: ", var_name, "\n", sep = "")
  
  lon <- data$coords$longitude
  lat <- data$coords$latitude
  
  nmonths <- data$dims[["time"]]
  nyears <- data$dims[["syear"]]
  ntimes <- nyears * nmonths
  
  out <- NULL
  
  for (x in seq_len(ntimes)) {
    
    m <- ((x - 1) %% nmonths) + 1
    y <- (((x - 1) %/% nmonths)) %% nyears + 1
    
    b <- substr(data$attrs$Dates[x], 1, 10)
    
    if (is.na(b)) {
      cat("Skipping missing date at index: ", x, "\n", sep = "")
      next
    }
    
    a <- data$data[1, 1, m, y, 1, , ]
    
    out[[b]] <- raster::raster(
      a,
      xmn = min(lon), xmx = max(lon),
      ymn = min(lat), ymx = max(lat)
    )
    
    raster::crs(out[[b]]) <- "EPSG:4326"
    
    if (x %% 25 == 0) {
      cat("  processed ", x, "/", ntimes, "\n", sep = "")
    }
  }
  
  raster_stack <- raster::stack(out)
  
  period_chunk <- stringr::str_extract(file, "\\d{6}-\\d{6}")
  
  out_file <- paste0(
    "temp_files/brazil_",
    var_name,
    "_",
    period_chunk,
    "_t2.rds"
  )
  
  saveRDS(raster_stack, out_file)
  
  cat("Saved: ", out_file, "\n", sep = "")
  
  rm(data, raster_stack, out)
  gc()
  
  invisible(out_file)
}

extract_chunk <- function(prlr_file, tas_file) {
  
  cat("Reading:\n", prlr_file, "\n", tas_file, "\n", sep = "")
  
  prlr_raster <- readRDS(prlr_file)
  tas_raster <- readRDS(tas_file)
  
  stopifnot(raster::nlayers(prlr_raster) == raster::nlayers(tas_raster))
  
  ntimestep <- raster::nlayers(prlr_raster)
  monthly_adm <- vector("list", ntimestep)
  
  for (l in seq_len(ntimestep)) {
    
    name <- names(prlr_raster[[l]])
    name <- substr(name, 2, 11)
    
    df1 <- data.frame(
      month = rep(substr(name, 6, 7), times = length(adm_id)),
      year = rep(substr(name, 1, 4), times = length(adm_id)),
      adm_id = adm_id,
      adm_name = adm_name,
      adm_latitude = adm_latitude
    )
    
    df1$prlr <- exactextractr::exact_extract(
      prlr_raster[[l]],
      adm,
      "mean",
      progress = FALSE
    )
    
    df1$tas <- exactextractr::exact_extract(
      tas_raster[[l]],
      adm,
      "mean",
      progress = FALSE
    )
    
    monthly_adm[[l]] <- df1
    
    if (l %% 25 == 0) {
      cat("  extracted ", l, "/", ntimestep, "\n", sep = "")
    }
  }
  
  out <- dplyr::bind_rows(monthly_adm)
  
  period_chunk <- stringr::str_extract(prlr_file, "\\d{6}-\\d{6}")
  
  out_file <- paste0(
    "temp_files/brazil_healthregions_tas_prlr_",
    period_chunk,
    ".csv"
  )
  
  write.csv(out, out_file, row.names = FALSE)
  
  cat("Saved: ", out_file, "\n", sep = "")
  
  rm(prlr_raster, tas_raster, monthly_adm, out)
  gc()
  
  invisible(out_file)
}

extract_forecast_nc <- function(nc_file, var_name) {
  
  nc <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  
  lon <- ncdf4::ncvar_get(nc, "longitude")
  lat <- ncdf4::ncvar_get(nc, "latitude")
  leadtime <- ncdf4::ncvar_get(nc, "leadtime")
  ensemble <- ncdf4::ncvar_get(nc, "ensemble")
  data <- ncdf4::ncvar_get(nc, "calibrated_forecast")
  
  results <- vector("list", length(leadtime) * length(ensemble))
  k <- 1
  
  for (l in seq_along(leadtime)) {
    for (e in seq_along(ensemble)) {
      
      r <- raster::raster(
        t(data[, , l, e]),
        xmn = min(lon), xmx = max(lon),
        ymn = min(lat), ymx = max(lat)
      )
      raster::crs(r) <- "EPSG:4326"
      
      values <- exactextractr::exact_extract(
        r,
        shape,
        "mean",
        append_cols = "regional_geocode",
        progress = FALSE
      )
      
      values$leadtime <- l
      values$ensemble <- e
      
      results[[k]] <- values
      k <- k + 1
      
      cat(
        var_name,
        " leadtime: ", l,
        " ensemble: ", e, "\n",
        sep = ""
      )
    }
  }
  
  result <- dplyr::bind_rows(results)
  
  out_file <- paste0(
    "out_files/brazil_monthly_fcst_",
    syear, sprintf("%02d", mm), "_", var_name, ".csv"
  )
  
  readr::write_csv(result, out_file)
  
  cat("Saved: ", out_file, "\n", sep = "")
  
  invisible(out_file)
}