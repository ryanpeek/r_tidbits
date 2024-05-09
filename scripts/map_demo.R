
# check out pacman
# install.packages("pacman")
library(pacman)
p_load(tidyverse, janitor, glue, sf, mapview,
       basemaps, tigris, terra, tidyterra)


# Libraries ---------------------------------------------------------------

library(readr)    # wrangle data and read it
library(dplyr)    # tidy data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)      # interactive maps!
mapviewOptions(fgb = FALSE)
library(basemaps) # basemaps like topo and satellite
library(tigris) # US boundaries
library(terra) # spatial raster vector package
library(tidyterra) # ggplot functions for terra

# the url for the Form data
form_data <- paste0("https://docs.google.com/spreadsheets/d/e/",
                    "2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-",
                    "qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-",
                    "/pub?gid=1462593645&single=true&output=csv")

# read in url and clean
dat <- readr::read_csv(form_data) |>
  janitor::clean_names() |>
  dplyr::rename( dining_name = 3, dining_address = 4)

# get states and counties
ca <- tigris::states() |> filter(STUSPS=="CA") |> st_transform(4326)
cnty <- tigris::counties(state = "CA") |> st_transform(4326)

# make into sf object so we can map
dat_geo <- dat |> rename(latitude=y_lat, longitude=x_lon) |>
  filter(!is.na(latitude) & !is.na(longitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326, remove = FALSE)

# crop to just ca pts
dat_geo_ca <- dat_geo[ ca,]

mapview(dat_geo_ca)

# set defaults
#set_defaults(map_service = "osm", map_type = "topographic")
#set_defaults(map_service = "esri", map_type = "natgeo_world_map")
set_defaults(map_service = "esri", map_type = "world_imagery")

# use terra to crop to area of interest
base_rast <- basemap_raster(ca, force = TRUE, verbose = FALSE)
base_terra <- terra::rast(base_rast) # convert to terra
# mask to just CA
base_terra_mask <- terra::crop(base_terra, st_transform(ca,3857),
                               mask=TRUE)

# map all CA
library(tidyterra)

base_map <-
  ggplot() +
  # cropped only
  geom_spatraster_rgb(data=base_terra_mask, alpha=0.8) +
  geom_sf(data=dat_geo_ca, fill="yellow", alpha=0.8, pch=21, col=alpha("gray40", 0.5), size=5) +
  geom_sf(data=cnty, fill=alpha("gray70", 0), color="gray90") +
  ggspatial::annotation_north_arrow(width = unit(1.2, "cm"), pad_y = unit(1.1, "cm")) +
  ggspatial::annotation_scale(unit_category="imperial") +
  theme_void()

base_map

# map just Sacramento
# use terra to crop to area of interest
cnty_rast <- basemap_raster(cnty[cnty$NAME=="Sacramento",], force = TRUE, verbose = FALSE)
cnty_terra <- terra::rast(cnty_rast) # convert to terra

# mask to just cnty
cnty_terra_mask <- terra::crop(cnty_terra, st_transform(cnty[cnty$NAME=="Sacramento",], 3857),
                               mask=TRUE)


cnty_map <-
  ggplot() +
  # cropped only
  geom_spatraster_rgb(data=cnty_terra_mask, alpha=0.8) +
  geom_sf(data=dat_geo_ca[cnty[cnty$NAME=="Sacramento",],], fill="yellow", alpha=0.8, pch=21, col=alpha("gray40", 0.5), size=5) +
  geom_sf(data=cnty[cnty$NAME=="Sacramento",], fill=alpha("gray70", 0), color="orange", lwd=1) +
  ggspatial::annotation_north_arrow(location="br", width = unit(1.2, "cm"), pad_y = unit(1.1, "cm")) +
  ggspatial::annotation_scale(unit_category="imperial", location="br") +
  theme_void()

cnty_map


library(tmap)
tmap_mode("plot")

tm1 <- tm_shape(cnty_terra_mask) +
  tm_rgb() +
  tm_layout(frame=FALSE, title = "Places to Eat in Sacramento County", fontface = "bold", fontfamily = "Roboto Condensed", title.position = c(0.5, 0.2)) +
  tm_compass(position = c("right","bottom"), size = 3) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_shape(dat_geo_ca[cnty[cnty$NAME=="Sacramento",],]) +
  tm_sf(col="yellow", shape=21, alpha=0.8, size = 1, border.col=alpha("gray30",0.5))
tm1

tmap_mode("view")
tm1
