# write to gpx file
library(sf)
library(tigris)
library(tidyverse)

# get CA counties and calc centroid
counties <- counties("CA")

cnty_cent <- st_centroid(counties)

# write to gpx
write_sf(cnty_cent, "data_out/cnty_centroids.gpx",
         dataset_options = "GPX_USE_EXTENSIONS=YES")
