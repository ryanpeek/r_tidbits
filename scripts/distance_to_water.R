# distance to water


# Libraries ---------------------------------------------------------------

library(sf)
library(nhdplusTools)
library(tidyverse)
library(nngeo)
# st_nn(a, b, k = 1, returnDist = T) # returns dist and nearest
library(rmapshaper)
library(mapview)
mapviewOptions(fgb=FALSE)

# Dataset -----------------------------------------------------------------

crs <- 3310 # set crs

# pt data
bird_pts <- read_csv("data_raw/site_locations_ecoregions.csv") |>
  st_as_sf(coords=c("Longitude","Latitude"), remove=FALSE, crs=4269) |>
  st_transform(crs)

# get boundary box
bbox_poly <- bird_pts |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(20000)

# make a 10km buffer around points and union
bird_buff_20 <- bird_pts |>
  st_buffer(20000) |>
  st_union()

# Get Flowlines Associated with Points ------------------------------------

# NHD path to full database:
db <- "/Users/rapeek/Documents/spatial_data/NHD_CA/NHD_H_California_State_GDB/NHD_H_California_State_GDB.gdb"

st_layers(db)
flowlines <- st_read(db, "NHDFlowline") |>
  st_transform(crs)
flowlines <- st_zm(flowlines)
flowlines_c <- flowlines[bird_buff_20,]

# get water body polygons
waterbodies <- st_read(db, "NHDWaterbody") |>
  st_transform(crs) |>
  st_zm()

# get flowlines...takes a minute with larger areas
# flowlines_plus <- nhdplusTools::get_nhdplus(bird_buff_20)
# subset_file <- "data_raw/nhdplus_data_bird_pts.gpkg"
# subset <- subset_nhdplus(comids = as.integer(flowlines$comid),
#                          output_file = subset_file,
#                          nhdplus_data = "download",
#                          flowline_only = FALSE,
#                          return_data = TRUE, overwrite = TRUE)

# crop to area of interest
#flowlines_plus_c <- flowlines_plus[bird_buff_20,]

# simplify fields a bit
#flowlines_c <- flowlines_plus_c |>
#  select(comid, gnis_id:streamorde, pathlength:arbolatesu, geometry) |>
#  st_transform(crs)

# Simplify ----------------------------------------------------------------

# flowlines_simple <- flowlines_c |>
#   st_transform(crs) |>
#   rmapshaper::ms_simplify(keep=0.1)

# water_bodies_simple <-
#   st_transform(water_bodies, crs) |>
#   st_buffer(0) |>
#   rmapshaper::ms_simplify(keep = 0.05)

# Nearest Lotic Feature -----------------------------------------

# See this code for options: https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf/51300037#51300037

# get nearest feature
nearest_lines <- flowlines_c[st_nearest_feature(bird_pts, flowlines_c),]

# get nearest point on nearest feature
nearest_lines_pts <- st_nearest_points(bird_pts, nearest_lines, pairwise=TRUE)

# convert to points
nearest_lines_pts <- st_cast(nearest_lines_pts, "POINT")[c(FALSE,TRUE)] |> st_as_sf()

# get distances
pt_dists <- st_distance(bird_pts, nearest_lines_pts, by_element = TRUE)

# bind
bird_pts$dist_m <-pt_dists

# map
mapview(nearest_lines_pts, col.regions="orange", cex=2)+
  mapview(bird_pts, zcol="dist_m") +
  mapview(nearest_lines, col.regions="blue", cex=4)
  #mapview(flowlines_simple, lwd=0.4)


# Nearest Lentic Feature -----------------------------------------

# get nearest feature
nearest_wb <- waterbodies[st_nearest_feature(bird_pts, waterbodies),]

# get nearest point on nearest feature
nearest_wb_pts <- st_nearest_points(bird_pts, nearest_wb, pairwise=TRUE)

# convert to points
nearest_wb_pts <- st_cast(nearest_wb_pts, "POINT")[c(FALSE,TRUE)] |> st_as_sf()

# get distances
pt_dists_wb <- st_distance(bird_pts, nearest_wb_pts, by_element = TRUE)

# bind
bird_pts$dist_m_wb <- pt_dists_wb

# map
mapview(nearest_wb_pts, col.regions="orange", cex=2)+
  mapview(bird_pts, zcol="dist_m_wb") +
  mapview(nearest_lines, col.regions="blue", cex=4) +
  mapview(nearest_wb, col.regions="cyan4", alpha.regions=0.5)


# Use NN Approach ---------------------------------------------------------

# this works but takes awhile
nearest_coords <- st_nn(bird_pts, flowlines_c, k = 1, returnDist = T)

# make a df and join
#nearest_df <- tibble("comid"=unlist(nearest_coords$nn), dist_m_nn=unlist(nearest_coords$dist), SiteID=bird_pts$SiteID)
#birds_dists2 <- left_join(bird_pts, nearest_df)

# or add directly
bird_pts$dist_m_nn <- unlist(nearest_coords$dist)

# save
saveRDS(bird_pts, file="data_out/bird_pts_w_dists.rda")
