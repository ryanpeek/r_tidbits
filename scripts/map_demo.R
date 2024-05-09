
# check out pacman to install lots of things at once
# install.packages("pacman")
# library(pacman)
#p_load(tidyverse, janitor, glue, sf, mapview,
#       basemaps, tigris, terra, tidyterra)


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readr)    # wrangle data and read it
library(dplyr)    # tidy data
library(janitor)      # clean column names
library(glue)         # modern paste() function
library(sf)           # make spatial data
library(mapview)      # interactive maps!
mapviewOptions(fgb = FALSE)

library(tigris) # US boundaries
options(tigris_use_cache = TRUE)

library(basemaps) # basemaps like topo and satellite
library(terra) # spatial raster vector package
library(tidyterra) # ggplot functions for terra
library(ggspatial) # north arrow, scale bars for ggplot

# Get the Data ------------------------------------------------------------

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

# Make Spatial ------------------------------------------------------------

# make into sf object so we can map
dat_geo <- dat |> rename(latitude=y_lat, longitude=x_lon) |>
  filter(!is.na(latitude) & !is.na(longitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326, remove = FALSE)

# crop to just points inside CA using brackets
dat_geo_ca <- dat_geo[ ca,]

# did it work?
mapview(dat_geo_ca)


# Get a Basemap! ----------------------------------------------------------

# set defaults for different versions
#set_defaults(map_service = "osm", map_type = "topographic")
#set_defaults(map_service = "esri", map_type = "natgeo_world_map")
set_defaults(map_service = "esri", map_type = "world_imagery")

# download the basemap for the area of interest (CA)
base_rast <- basemap_raster(ca, force = TRUE, verbose = FALSE)

# convert to a terra spatial raster format
base_terra <- terra::rast(base_rast)

# mask to just CA
# (cropping only clips to the bounding box, mask removes everything outside of the polygon of interest)
base_terra_mask <- terra::crop(base_terra, st_transform(ca,3857),
                               mask=TRUE)

# map all CA
library(tidyterra)

# Map with {ggplot} --------------------------------------------------------

# make a ggplot with basemap
(gg_base_map <-
  ggplot() +
  # add the raster basemap first
  geom_spatraster_rgb(data=base_terra_mask, alpha=0.8) +
  # plot counties
  geom_sf(data=cnty, fill=alpha("gray70", 0), color="gray90") +
  # plot points
  geom_sf(data=dat_geo_ca, fill="yellow", alpha=0.8, pch=21, col=alpha("gray40", 0.5), size=5) +
  # add north arrow
  ggspatial::annotation_north_arrow(width = unit(1.2, "cm"), pad_y = unit(1.1, "cm")) +
  # add scale bar
  ggspatial::annotation_scale(unit_category="imperial") +
  # add a title
  labs(title = "Places to Eat in California") +
  theme_void()
 )
# using parenthesis around the plotting code will plot object so you don't have
# to rerun "gg_base_map" to see the plot

## Map a Specific County --------------------------------------------------

# pick a county
county_select <- "Sacramento"

# get basemap and make a spatraster
cnty_rast <- basemap_raster(cnty[cnty$NAME==county_select,],
                            force = TRUE, verbose = FALSE)
cnty_terra <- terra::rast(cnty_rast) # convert to terra

# mask to county
cnty_terra_mask <- terra::crop(cnty_terra,
                               st_transform(cnty[cnty$NAME==county_select,], 3857),
                               mask=TRUE)

# county map
(gg_cnty_map <-
    ggplot() +
    # cropped only
    geom_spatraster_rgb(data=cnty_terra_mask, alpha=0.8) +
    geom_sf(data=dat_geo_ca[cnty[cnty$NAME=="Sacramento",],], fill="yellow", alpha=0.8, pch=21, col=alpha("gray40", 0.5), size=5) +
    geom_sf(data=cnty[cnty$NAME=="Sacramento",], fill=alpha("gray70", 0), color="orange", lwd=1) +
    ggspatial::annotation_north_arrow(location="br", width = unit(1.2, "cm"), pad_y = unit(1.1, "cm")) +
    ggspatial::annotation_scale(unit_category="imperial", location="br") +
    labs(title = glue("Places to Eat in {county_select} County"))+
    theme_void()
)

# Map with {tmap} ---------------------------------------------------------

library(tmap)

## Static Plot -----------------------------------------------------------

# can make plots static (plot) or interactive (view) with same code!

# set the mode of the plot
tmap_mode("plot")

# tmap is similar to ggplot
(tm_cnty_map <-
    # the main difference is needing tm_shape() in front of each spatial object
    tm_shape(cnty_terra_mask) +
    # followed by the type of layer you want to plot
    tm_rgb() +
    # some fancy options
    tm_layout(frame=FALSE,
              title = glue("Places to Eat in {county_select}"),
              fontface = "bold",
              title.position = c(0.45, 0.24)) +
    # add a compass and scale bar
    tm_compass(position = c("right","bottom"), size = 3) +
    tm_scale_bar(position = c("right", "bottom")) +
    # add our points!
    tm_shape(dat_geo_ca[cnty[cnty$NAME==county_select,],]) +
    tm_sf(col="yellow", shape=21, alpha=0.8, size = 1,
          border.col=alpha("gray30",0.5))
)


## Now make an interactive version ----------------------------------------

# it's simple!
tmap_mode("view")
tm_cnty_map
