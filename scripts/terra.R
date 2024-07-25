# reading rasters

library(terra)
library(tidyterra)
library(tidyverse)
library(sf)

# path
db <- r'(C:\Users\RPeek\Downloads\Watershed_Metric_Resources_v5.gdb)'

st_layers(db) # gives list of vectors in database

terra::vector_layers(db)
terra::rast(db,"1")
ras <- terra::rast(db, "NLCD_Impervious_2016")
names(ras)

plot(ras)

