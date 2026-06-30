# Script name: 06_climatologies.R
#
# Purpose of script: Get climatologies (tas, prlr, enso) per health region and month
# Author: Carles Milà
# Date Created: 2025-07-29

# tas and prlr ----

# Read observed data 
obs <- read.csv("temp_files/brazil_healthregions_tas_prlr_195001-202512.csv")

# Compute climatologies 
clim_tas <- aggregate(tas ~ adm_id + month,
                           data = obs[obs$year >= 1991 & obs$year <= 2020,],
                           FUN = function(x) c(mean = mean(x)))
clim_tas <- do.call(data.frame, clim_tas)


clim_prlr <- aggregate(prlr ~ adm_id + month,
                      data = obs[obs$year >= 1991 & obs$year <= 2020,],
                      FUN = function(x) c(mean = mean(x)))
clim_prlr <- do.call(data.frame, clim_prlr)

clims <- merge(clim_tas, clim_prlr, by=c("adm_id","month"))


# enso ----
ref_iyear <- 1981
ref_fyear <- 2010
iyear <- 1981
fyear <- 2025

# function to get oni and nino
get_oni <- function(ref_iyear, ref_fyear, iyear, fyear, source = "short") {
  
  # get the correct  dataset
  if (source == "short") {
    data <- read.csv("https://psl.noaa.gov/data/correlation/nina34.data",
                     header = TRUE)
  } else if (source == "long") {
    data <- read.csv("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino34.long.data",
                     header = TRUE)
  } else {
    print("Invalid source.")
    break
  }
  
  # get list of all years
  allyears <- seq(as.numeric(scan(text = data[[1]][1],
                                  what = "", quiet = TRUE))[1],
                  lubridate::year(Sys.time()),
                  1)
  
  # calcualte the mean of reference period
  refperiod <- data.frame()
  for (y in match(ref_iyear, allyears):match(ref_fyear, allyears)){
    line <- as.numeric(scan(text = data[[1]][y], what = "", quiet = TRUE))
    refperiod <- rbind(refperiod, c(line[2:13]))
  }
  ref <- colMeans(refperiod)
  
  # get SST and calculate anomalies (i.e. nino34)
  df <- data.frame()
  for (y in match(iyear, allyears):match(fyear, allyears)){
    line <- as.numeric(scan(text = data[[1]][y], what = "", quiet = TRUE))
    for (m in 1:12){
      df <- rbind(df, c(line[1], m, round(line[m + 1] - ref[m], 2)))
    }
  }
  colnames(df) <- c("year", "month", "nino34")
  
  # define last month and delete last few rows
  last_line <- as.numeric(scan(text = data[[1]][match(fyear, allyears)],
                               what = "", quiet = TRUE))
  last_month <- tail(which(last_line != -99.99), 1) - 1
  df <- subset(df, !(year == fyear & month > last_month))
  
  # calculate the 3 month rolling mean (i.e. ONI)
  df$oni <- round(zoo::rollmean(df$nino34, 3, align = "right", fill = NA), 2)
  
  # return result
  return(df)
}

nino <- get_oni(ref_iyear, ref_fyear, iyear, fyear, source = "short")

clim_nino <- aggregate(nino34 ~ month,
                       data = nino[nino$year >= 1991 & nino$year <= 2020,],
                       FUN = function(x) c(mean = round(mean(x), 2)))
clim_nino <- do.call(data.frame, clim_nino)

clims <- merge(clims, clim_nino, by="month")

# Write to disk ----
write.csv(clims, "temp_files/climatologies_199101-202012.csv", row.names = FALSE)
