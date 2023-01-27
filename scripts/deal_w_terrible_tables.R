# SFWP data


# Libraries --------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(mapview)
mapviewOptions(fgb=FALSE)

# Get Data ----------------------------------------------------------------

fpath <- "data_raw/SFWP_site_utm.csv"
#fpath <- r'(C:\Users\RPeek\OneDrive - California Department of Fish and Wildlife\Documents\DATA\sfwp\SFWP_site_utm.csv)'

utms <- fread(fpath) # try fread for weird formats
# now pull 51-74:
utms_sc <- utms[51:74,]

# drop blank/empty cols
utms_sc <- janitor::remove_empty(utms_sc, "cols")

# fix colnames
colnames(utms_sc) <- c("Site_RM", "site_id", "UTM", "northing", "easting", "suitability")

# convert
utms_sc_sf <- utms_sc %>% select(1:5) %>%
  st_as_sf(coords = c( "northing", "easting"), crs=26710, remove=FALSE)

mapview(utms_sc_sf) # that worked!

utms_filt <- utms_sc_sf %>% filter(site_id %in% c("SCD20", "SCD1"))

mapview(utms_sc_sf) + mapview(utms_filt, col.regions="orange")
