# Script name: 03_obs_nino_oni
#
# Calculate the nino3.4 and oni indices for the sprint challenge on a health 
# region level.
# 
# Author: Carles Milà (adapted from Daniela Lührsen's script)
# Date Created: 2025-06-03
# Email: carles.milagarcia@bsc.es
#
# Env: HUB with module R-bundle-CRAN/2024.06-foss-2023b


## load up the packages
# packages <- c("zoo", "lubridate")
# lapply(packages, require, character.only = TRUE)

# ## ---------------------------
# ref_iyear <- 1981
# ref_fyear <- 2010
# iyear <- 2008
# fyear <- 2025

# # function to get oni and nino
# get_oni <- function(ref_iyear, ref_fyear, iyear, fyear, source = "short") {

#   # get the correct  dataset
#   if (source == "short") {
#     data <- read.csv("https://psl.noaa.gov/data/correlation/nina34.data",
#                      header = TRUE)
#   } else if (source == "long") {
#     data <- read.csv("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino34.long.data",
#                      header = TRUE)
#   } else {
#     print("Invalid source.")
#     break
#   }

#   # get list of all years
#   allyears <- seq(as.numeric(scan(text = data[[1]][1],
#                                   what = "", quiet = TRUE))[1],
#                   lubridate::year(Sys.time()),
#                   1)

#   # calcualte the mean of reference period
#   refperiod <- data.frame()
#   for (y in match(ref_iyear, allyears):match(ref_fyear, allyears)){
#     line <- as.numeric(scan(text = data[[1]][y], what = "", quiet = TRUE))
#     refperiod <- rbind(refperiod, c(line[2:13]))
#   }
#   ref <- colMeans(refperiod)

#   # get SST and calculate anomalies (i.e. nino34)
#   df <- data.frame()
#   for (y in match(iyear, allyears):match(fyear, allyears)){
#     line <- as.numeric(scan(text = data[[1]][y], what = "", quiet = TRUE))
#     for (m in 1:12){
#       df <- rbind(df, c(line[1], m, round(line[m + 1] - ref[m], 2)))
#     }
#   }
#   colnames(df) <- c("year", "month", "nino34")

#   # define last month and delete last few rows
#   last_line <- as.numeric(scan(text = data[[1]][match(fyear, allyears)],
#                                what = "", quiet = TRUE))
#   last_month <- tail(which(last_line != -99.99), 1) - 1
#   df <- subset(df, !(year == fyear & month > last_month))

#   # calculate the 3 month rolling mean (i.e. ONI)
#   df$oni <- round(zoo::rollmean(df$nino34, 3, align = "right", fill = NA), 2)

#   # return result
#   return(df)
# }

# ## the following line does not work in the workstation
# # apply function with desired referenc period
# nino <- get_oni(ref_iyear, ref_fyear, iyear, fyear, source = "short")

# # get start and end month for correct file naming
# imonth <- min(nino$month[nino$year == iyear])
# fmonth <- max(nino$month[nino$year == fyear])

# # save output
# write.csv(nino, paste0("climate/out_files/brazil_monthly_hist_nino_",
#                        iyear, sprintf("%02d", imonth), "-",
#                        fyear, sprintf("%02d", fmonth), ".csv"),
#           row.names = FALSE)

packages <- c("zoo", "lubridate", "dplyr", "purrr", "readr")
lapply(packages, require, character.only = TRUE)

ref_iyear <- 1981
ref_fyear <- 2010
iyear <- 2008
fyear <- 2025

get_oni <- function(ref_iyear, ref_fyear, iyear, fyear, source = "short") {
  
  url <- dplyr::case_when(
    source == "short" ~ "https://psl.noaa.gov/data/correlation/nina34.data",
    source == "long" ~ "https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino34.long.data",
    TRUE ~ NA_character_
  )
  
  if (is.na(url)) {
    stop("Invalid source: ", source)
  }
  
  data <- tryCatch(
    read.csv(url, header = TRUE),
    error = function(e) {
      stop("Could not read Niño data from NOAA. Check internet access. Error: ", e$message)
    }
  )
  
  first_year <- as.numeric(scan(text = data[[1]][1], what = "", quiet = TRUE))[1]
  allyears <- seq(first_year, lubridate::year(Sys.time()), 1)
  
  read_year_line <- function(year) {
    line <- data[[1]][match(year, allyears)]
    as.numeric(scan(text = line, what = "", quiet = TRUE))
  }
  
  refperiod <- purrr::map(ref_iyear:ref_fyear, \(year) {
    read_year_line(year)[2:13]
  }) |>
    do.call(what = rbind)
  
  ref <- colMeans(refperiod, na.rm = TRUE)
  
  df <- purrr::map_dfr(iyear:fyear, \(year) {
    line <- read_year_line(year)
    
    tibble::tibble(
      year = line[1],
      month = 1:12,
      nino34 = round(line[2:13] - ref, 2)
    )
  })
  
  last_line <- read_year_line(fyear)
  last_month <- tail(which(last_line != -99.99), 1) - 1
  
  df |>
    dplyr::filter(!(year == fyear & month > last_month)) |>
    dplyr::mutate(
      oni = round(zoo::rollmean(nino34, 3, align = "right", fill = NA), 2)
    )
}

nino <- get_oni(ref_iyear, ref_fyear, iyear, fyear, source = "short")

imonth <- min(nino$month[nino$year == iyear])
fmonth <- max(nino$month[nino$year == fyear])

readr::write_csv(
  nino,
  paste0(
    "climate/out_files/brazil_monthly_hist_nino_",
    iyear, sprintf("%02d", imonth), "-",
    fyear, sprintf("%02d", fmonth), ".csv"
  )
)


### END
