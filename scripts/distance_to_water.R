# distance to water


# Libraries ---------------------------------------------------------------

library(sf)
library(nhdplusTools)
library(tidyverse)
library(nngeo)
library(rmapshaper)
library(mapview)
mapviewOptions(fgb=FALSE)

# Dataset -----------------------------------------------------------------

crs <- 3310 # set crs

# get bird point data
bird_pts <- read_csv("data_raw/site_locations_ecoregions.csv") |>
  st_as_sf(coords=c("Longitude","Latitude"), remove=FALSE, crs=4269) |>
  st_transform(crs)

# get boundary box around all points, used for cropping data
bbox_poly <- bird_pts |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(20000) # use 20km buffer

# make a 20km buffer around each point and dissolve into one
bird_buff_20 <- bird_pts |>
  st_buffer(20000) |>
  st_union()

# Get Flowlines Associated with Points ------------------------------------

### run this from scratch if you want, but takes a while

# NHD path to full database:
db <- r'(S:\GIS_Library\Hydrography\NHD\High_Resolution\NHD_H_California_State_GDB.gdb)'

#db <- "/Users/rapeek/Documents/spatial_data/NHD_CA/NHD_H_California_State_GDB/NHD_H_California_State_GDB.gdb"

st_layers(db)

# FYI THIS TAKES ABOUT 4-5 minutes
flowlines <- st_read(db, "NHDFlowline") |>
  st_transform(crs)

flowlines <- st_zm(flowlines) # drop the z elevation
flowlines_c <- flowlines[bird_buff_20,] # crop to area of interest

# get water body polygons, also takes a minute
waterbodies <- st_read(db, "NHDWaterbody") |>
  st_transform(crs) |>
  st_zm()

wb_c <- waterbodies[bird_buff_20,]

# save out these layers:
save(flowlines_c, wb_c, file = "data_raw/nhd_clipped_for_bird_dists.rda")


# Load Flowline and Water Body Data outputs from Section Above ---------------

# change this path to match your path
load(r'(C:\Users\RPeek\OneDrive - California Department of Fish and Wildlife\Documents\PROJECTS\CANNABIS\bird_distances\data\nhd_clipped_for_bird_dists.rda)')

# Simplify ----------------------------------------------------------------
# can avoid this step for now, but helps simplify the geometry and
# makes processing the data faster, but lose some potential accuracy in lines

# flowlines_simple <- flowlines_c |>
#   st_transform(crs) |>
#   rmapshaper::ms_simplify(keep=0.1)

# water_bodies_simple <-
#   st_transform(water_bodies, crs) |>
#   st_buffer(0) |>
#   rmapshaper::ms_simplify(keep = 0.05)

# Nearest Lotic Feature -----------------------------------------

# get nearest feature
nearest_lines <- flowlines_c[st_nearest_feature(bird_pts, flowlines_c),]

# get nearest point on nearest feature
nearest_lines_feat <- st_nearest_points(bird_pts, nearest_lines, pairwise=TRUE)

# convert to points
nearest_lines_pts <- st_cast(nearest_lines_feat, "POINT")[c(FALSE,TRUE)] |> st_as_sf()

# get distances
pt_dists <- st_distance(bird_pts, nearest_lines_pts, by_element = TRUE)

# bind
bird_pts$dist_m <-pt_dists

# map
mapview(nearest_lines_pts, col.regions="orange", cex=2)+
  mapview(bird_pts, zcol="dist_m") +
  mapview(nearest_lines, col.regions="blue", cex=4) +
  mapview(nearest_lines_feat, color="gold", cex=0.5)


# Nearest Lentic Feature -----------------------------------------

# get nearest feature
nearest_wb <- wb_c[st_nearest_feature(bird_pts, wb_c),]

# get line to nearest point on nearest feature
nearest_wb_lines <- st_nearest_points(bird_pts, nearest_wb, pairwise=TRUE)

# convert to points
nearest_wb_pts <- st_cast(nearest_wb_lines, "POINT")[c(FALSE,TRUE)] |> st_as_sf()

# get distances
pt_dists_wb <- st_distance(bird_pts, nearest_wb_pts, by_element = TRUE)

# bind
bird_pts$dist_m_wb <- pt_dists_wb

# map
mapview(nearest_wb_pts, col.regions="orange", cex=2)+
  mapview(bird_pts, zcol="dist_m_wb") +
  mapview(nearest_wb_lines, col.regions="blue", cex=4) +
  mapview(nearest_wb, col.regions="cyan4", alpha.regions=0.5)


# Use NN Approach ---------------------------------------------------------

# this works but takes awhile, different approach
# nearest_coords <- st_nn(bird_pts, flowlines_c, k = 1, returnDist = T)

# or add directly
# bird_pts$dist_m_nn <- unlist(nearest_coords$dist)


# Save Out ----------------------------------------------------------------

# remove the metric side of things and save out
bird_pts_out <- bird_pts |>
  mutate(dist_m = as.numeric(dist_m),
         dist_m_wb = as.numeric(dist_m_wb)) |>
  select(SiteID, WatershedC, starts_with("dist"), everything())

# save
saveRDS(bird_pts_out, file="data_out/bird_pts_w_dists.rda")
write_csv(bird_pts_out, "data_out/bird_pts_w_dists.csv")
