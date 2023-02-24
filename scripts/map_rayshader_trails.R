# rayshader trail map
# see this post: https://www.tylermw.com/adding-open-street-map-data-to-rayshader-maps-in-r/

library(rayshader)
library(raster)
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)

bryce = raster("Bryce_Canyon.tif")
bryce_mat = raster_to_matrix(bryce)
