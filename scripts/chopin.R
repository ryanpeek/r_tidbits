# read in shp

library(terra)
library(sf)
library(tidyverse)
library(pacman)
p_load("chopin", "future", "future.mirai", "mirai", "h3r", "dggridR")

# get shp:
shp <- st_read("~/Downloads/casc_frog_range_wgs84/casc_frog_range_wgs84.shp")

# using chopin (https://docs.ropensci.org/chopin/index.html)

rlang::check_installed("amadeus")

tcli_variables <- c(
  "def","ppt", "q", "swe",
  "PDSI", "tmax", "tmin"
)

amadeus::download_terraclimate(
  variables = tcli_variables,
  year = c(2000, 2024),
  directory_to_save = "data_terraclimate/",
  acknowledgement = TRUE,
  download = TRUE,
  remove_command = TRUE
)

options(future.globals = FALSE)
# some bands should be summed
bandnames <- c(
  "def", "PDSI", "ppt", "q",
  "swe", "tmax", "tmin"
)
bandnames_sorted <- sort(bandnames)

# single nc file, yearly aggregation by fun value
# band for summation
bandnames_sum <- c("def", "ppt", "q", "swe")

# band for averaging
bandnames_avg <- c("PDSI", "tmax", "tmin")

# mean: temporally marginal pixel mean (i.e., monthly -> yearly)
# sum: temporally marginal pixel sum (i.e., monthly -> yearly)
# Preprocessed data are stored in
tictoc::tic("sum: 7 layers")
netcdf_read_sum <-
  split(bandnames_sum, bandnames_sum) |>
  lapply(function(x) {
    grep(paste0("(", x, ")"), path_tc_files, value = TRUE)
  }) |>
  lapply(function(x) {
    terra::tapp(terra::rast(x, win = ext_mainland, snap = "out"), index = "years", fun = "sum")
  })
netcdf_read_sum <- Reduce(c, netcdf_read_sum)
tictoc::toc()




