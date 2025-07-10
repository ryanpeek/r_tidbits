#read and convert a kml/kmz
library(sf)
library(tidyverse)
library(glue)

# file name: note if a kmz, rename as kml (or unzip and make kml):
# command line to unzip to "kmls" folder:
# unzip -d kmls myfile.kmz
# then rename doc.kml
filename <- "myfile.kml"

# see layers
st_layers(filename)
st_layers(filename)[[1]]

# pull in layers
rb1 <- st_read(filename, st_layers(filename)[[1]][1]) %>% st_zm()
#rb2 <- st_read(filename, st_layers(filename)[[1]][2]) %>% st_zm()

# write to gpx...need to match names
pheno |> rename("name"=site_id) |>
  write_sf("data_out/pheno_sites.gpx",driver = "GPX",
           dataset_options = "GPX_USE_EXTENSIONS=YES")

# map it
library(mapview)
mapviewOptions(fgb = FALSE)

mapview(rb1, col.regions="orange") + mapview(rb2)

# export it
st_write(rb1, dsn = "~/Downloads/kmls/rb1.shp")
st_write(rb2, dsn = "~/Downloads/kmls/rb2.shp")

st_write(rb1, dsn = "~/Downloads/kmls/rb1.kml", delete_dsn = TRUE)
st_write(rb2, dsn = "~/Downloads/kmls/rb2.kml", delete_dsn = TRUE)
