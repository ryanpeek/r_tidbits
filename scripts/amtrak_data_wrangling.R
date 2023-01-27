# amtrack data

# see this github wrangling by jfangmeier: https://github.com/jfangmeier/table-contest-2022/blob/main/R/data-prep.qmd

# here: https://data-usdot.opendata.arcgis.com/datasets/usdot::amtrak-stations/about
# here: https://data-usdot.opendata.arcgis.com/datasets/usdot::amtrak-routes/about

library(tidyverse)
library(sf)
library(rvest)
library(readxl)
library(tigris)
library(osmdata)
library(fuzzyjoin)

amtrak_rout_sf <-
  st_read(here::here("data-raw", "Amtrak_Routes.geojson"))
amtrak_stat_sf <-
  st_read(here::here("data-raw", "Amtrak_Stations.geojson"))

