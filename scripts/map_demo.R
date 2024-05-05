
library(tidyverse)    # wrangle data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)      # interactive maps!
mapviewOptions(fgb = FALSE)
library(basemaps)

# the url for the Form data
form_data <- paste0("https://docs.google.com/spreadsheets/d/e/",
                    "2PACX-1vSODxBm_z5Gu8a42C6ZFEa3S5iTbYV-",
                    "qucCGvasGS6c0qFUAml5vSMEgbvI9PYo1HJ20Y_WY62aTAb-",
                    "/pub?gid=1462593645&single=true&output=csv")

# read in url and clean
dat <- read_csv(form_data) |>
  clean_names() |>
  rename( dining_name = 3, dining_address = 4)

# get states and counties
ca <- tigris::states() |> filter(STUSPS=="CA") |> st_transform(4326)
cnty <- tigris::counties(state = "CA") |> st_transform(4326)

# make into sf object so we can map
dat_geo <- dat |> rename(latitude=y_lat, longitude=x_lon) |>
  filter(!is.na(latitude) & !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# crop to just ca pts
dat_geo_ca <- dat_geo[ ca,]

# set defaults
set_defaults(map_service = "osm", map_type = "topographic")
set_defaults(map_service = "esri", map_type = "world_imagery")
set_defaults(map_service = "esri", map_type = "natgeo_world_map")

# use terra to crop to area of interest
base_rast <- basemap_raster(ca, force = TRUE, verbose = FALSE,
                            map_service = "esri",
                            map_type = "world_imagery")
base_terra <- terra::rast(base_rast) # convert to terra
# mask to just CA
base_terra_mask <- terra::crop(base_terra, st_transform(ca,3857),
                               mask=TRUE)

# map
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
