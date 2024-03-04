# calculate distance to water from random points

# Libraries ---------------------------------------------------------------

library(sf)
library(nhdplusTools)
library(tidyverse)
library(nngeo)
library(rmapshaper)
library(mapview)
mapviewOptions(fgb=FALSE)
library(tigris)

# use metric UTM
crs <- 3310 # set crs

# Dataset -----------------------------------------------------------------

# get counties
cnty <- counties("CA")
cnty <- st_transform(cnty, crs)

# pick one
cnty_sel <- cnty |> filter(NAME=="Fresno")

# now download nhdplus rivers for this area:
flowlines_plus <- nhdplusTools::get_nhdplus(cnty_sel)
subset_file <- "data_raw/nhdplus_fresno.gpkg"
subset <- subset_nhdplus(comids = as.integer(flowlines_plus$comid),
                         output_file = subset_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

# see different flow types:
table(subset$NHDFlowline_Network$ftype)
table(subset$NHDFlowline_NonNetwork$ftype)
table(subset$NHDWaterbody$ftype)

# get one layer
flowlines <- subset$NHDFlowline_Network |>
  st_transform(crs)

# crop to just a given HUC
h10_sel <- "1803001003" # MF Kings
h10 <- get_huc(cnty_sel, type="huc10")
# filter
h10_sel_sf <- h10 |> filter(huc10==h10_sel)

# crop to area of interest
flowlines_c <- flowlines[h10_sel_sf,]
wb <- subset$NHDWaterbody |> st_transform(crs) |>
  st_zm()
wb_c <- wb[h10_sel_sf,]

# simplify fields a bit
flowlines_c <- flowlines_c |>
  select(comid, gnis_id:streamorde,
         hydroseq:arbolatesu, geometry) |>
  st_transform(crs) |>
  st_zm()

# make some random point in the huc
pts <- st_sample(x = h10_sel_sf, size = 100)

# get boundary box w 5km buffer
bbox_poly <- h10_sel_sf |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(5000)

# check
mapview(bbox_poly, alpha.regions=0, lwd=3) + mapview(pts) + mapview(flowlines_c, cex=0.3) +
  mapview(wb_c, col.regions="cyan4", alpha.regions=0.4)


# Simplify ----------------------------------------------------------------

# only if you want to simplify the flowlines a bit

# flowlines_simple <- flowlines_c |>
#   st_transform(crs) |>
#   rmapshaper::ms_simplify(keep=0.1)

# wb_simple <-
#   st_transform(water_bodies, crs) |>
#   st_buffer(0) |>
#   rmapshaper::ms_simplify(keep = 0.05)

# Nearest Lotic Feature -----------------------------------------

# get nearest feature
nearest_feat <- flowlines_c[st_nearest_feature(pts, flowlines_c),]

# get nearest point on nearest feature
nearest_feat_lines <- st_nearest_points(pts, nearest_feat, pairwise=TRUE)

# convert to points
nearest_feat_pts <- st_cast(nearest_feat_lines, "POINT")[c(FALSE,TRUE)] |> st_as_sf()

# get distances
pt_dists <- st_distance(pts, nearest_feat_pts, by_element = TRUE)

# bind
pts_sf <- st_as_sf(pts) |>
  rownames_to_column("ID") |>
  rename(geometry=x) |>
  mutate(dist_m = pt_dists)

# map the nearest line pts
mapview(nearest_feat_lines, col.regions="gray20", lwd=0.7)+
  mapview(nearest_feat_pts, col.regions="orange", cex=3)+
  mapview(pts_sf, zcol="dist_m") +
  mapview(flowlines_c, col.regions="blue", cex=0.5)

# Nearest Lentic Feature -----------------------------------------

# get nearest feature
nearest_wb <- wb_c[st_nearest_feature(pts, wb_c),]

# get nearest point on nearest feature
nearest_wb_lines <- st_nearest_points(pts, nearest_wb, pairwise=TRUE)

# convert to points
nearest_wb_pts <- st_cast(nearest_wb_lines, "POINT")[c(FALSE,TRUE)] |> st_as_sf()

# get distances
pt_dists_wb <- st_distance(pts, nearest_wb_pts, by_element = TRUE)

# bind
pts_sf$dist_m_wb <- pt_dists_wb

# map
mapview(nearest_wb_lines, color="orange", cex=0.5)+
mapview(nearest_wb_pts, col.regions="black", cex=3)+
  mapview(pts_sf, zcol="dist_m_wb", cex=4) +
  mapview(nearest_wb, col.regions="cyan4", alpha.regions=0.5)

# Use NN Approach ---------------------------------------------------------

# this works but takes awhile
nearest_coords <- st_nn(pts, flowlines_c, k = 1, returnDist = T)

# make a df and join
#nearest_df <- tibble("comid"=unlist(nearest_coords$nn), dist_m_nn=unlist(nearest_coords$dist), SiteID=bird_pts$SiteID)
#birds_dists2 <- left_join(bird_pts, nearest_df)

# or add directly
pts_sf$dist_m_nn <- unlist(nearest_coords$dist)
