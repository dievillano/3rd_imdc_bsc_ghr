# # Script name: 04_obs_merge.R
# #
# # Merge all observed variables into one dataset
# # 
# # Author: Carles Milà (adapted from Daniela Lührsen's script)
# # Date Created: 2025-06-06
# # Email: carles.milagarcia@bsc.es
# #
# # Env: HUB with module R-bundle-CRAN/2024.06-foss-2023b

# # load data
# climate <-  read.csv(paste0("climate/out_files/brazil_monthly_hist_tas_prlr_",
#                             "200801-202505.csv"))
# spi <-      read.csv(paste0("climate/out_files/brazil_monthly_hist_spi_",
#                             "195001-202512.csv"))
# nino <-     read.csv(paste0("climate/out_files/brazil_monthly_hist_nino_",
#                             "200801-202505.csv"))
# names(nino) <- gsub("nino34", "nino", names(nino), fixed = TRUE)

# # merge data
# df <- merge(climate, spi, by = c("month", "year", "adm_id"))
# df <- merge(df, nino, by = c("month", "year"))

# # save output
# start_year <- min(df$year)
# start_month <- min(df$month[df$year==start_year])
# end_year <- max(df$year)
# end_month <- max(df$month[df$year==end_year])
# write.csv(df,
#           paste0("output/brazil_monthly_hist_",
#                  start_year, sprintf("%02d", start_month), "-",
#                  end_year, sprintf("%02d", end_month), ".csv"),
#           row.names = FALSE)

# Script name: 04_obs_merge.R

library(dplyr)
library(readr)

climate_file <- "climate/out_files/brazil_monthly_hist_tas_prlr_200801-202512.csv"
spi_file <- "climate/out_files/brazil_monthly_hist_spi_195001-202512.csv"
nino_file <- "climate/out_files/brazil_monthly_hist_nino_200801-202512.csv"

stopifnot(file.exists(climate_file))
stopifnot(file.exists(spi_file))
stopifnot(file.exists(nino_file))

climate <- readr::read_csv(climate_file, show_col_types = FALSE)
spi <- readr::read_csv(spi_file, show_col_types = FALSE)
nino <- readr::read_csv(nino_file, show_col_types = FALSE) |>
  dplyr::rename(
    nino = nino34
  )

climate <- climate |>
  dplyr::mutate(
    month = as.integer(month),
    year = as.integer(year),
    adm_id = as.character(adm_id)
  )

spi <- spi |>
  dplyr::mutate(
    month = as.integer(month),
    year = as.integer(year),
    adm_id = as.character(adm_id)
  )

nino <- nino |>
  dplyr::mutate(
    month = as.integer(month),
    year = as.integer(year)
  )

df <- climate |>
  dplyr::left_join(
    spi,
    by = c("month", "year", "adm_id")
  ) |>
  dplyr::left_join(
    nino,
    by = c("month", "year")
  ) |>
  dplyr::arrange(adm_id, year, month)

start_year <- min(df$year, na.rm = TRUE)
start_month <- min(df$month[df$year == start_year], na.rm = TRUE)
end_year <- max(df$year, na.rm = TRUE)
end_month <- max(df$month[df$year == end_year], na.rm = TRUE)

out_file <- paste0(
  "climate/out_files/brazil_monthly_hist_",
  start_year, sprintf("%02d", start_month), "-",
  end_year, sprintf("%02d", end_month), ".csv"
)

readr::write_csv(df, out_file)

cat("Saved: ", out_file, "\n", sep = "")


# ### END
