# distance to water


# Libraries ---------------------------------------------------------------

library(sf)
library(nhdplusTools)
library(tidyverse)
library(nngeo)
library(rmapshaper)
library(mapview)
mapviewOptions(fgb=FALSE)
# st_nn(a, b, k = 1, returnDist = T) # returns dist and nearest


# Dataset -----------------------------------------------------------------

# pt data
bird_pts <- read_csv("data_raw/site_locations_ecoregions.csv") |>
  st_as_sf(coords=c("Longitude","Latitude"), remove=FALSE, crs=4269) |>
  st_transform(3310)

# get boundary box
bbox_poly <- bird_pts |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(20000)

# Get Flowlines Associated with Points ------------------------------------

# get flowlines...takes a minute with larger areas
flowlines <- nhdplusTools::get_nhdplus(bbox_poly)

# can use this to grab COMIDs and then waterbodies
# subset_file <- "data_raw/nhdplus_data_bird_pts.gpkg"
# subset <- subset_nhdplus(comids = as.integer(flowlines$comid),
#                          output_file = subset_file,
#                          nhdplus_data = "download",
#                          flowline_only = FALSE,
#                          return_data = TRUE, overwrite = TRUE)


flowlines_s <- flowlines |>
  select(comid, gnis_id:streamorde, pathlength:arbolatesu, geometry)



# Simplify ----------------------------------------------------------------

crs <- 3310

flowlines_simple <- flowlines_s |>
  st_transform(crs) |>
  rmapshaper::ms_simplify(keep=0.1)

# water_bodies_simple <-
#   st_transform(water_bodies, crs) |>
#   st_buffer(0) |>
#   rmapshaper::ms_simplify(keep = 0.05)

# Nearest -----------------------------------------------------------------

nearest_pts <- st_nearest_points(bird_pts, flowlines_simple)

# get nearest feature
nearest_pts <- st_nearest_feature(bird_pts, flowlines_simple)

# Snap geometries of POIs to the network
snapped_pts <- bird_pts  |>
  st_set_geometry(st_geometry(flowlines_simple)[nearest_pts])

# then, convert the LINESTRING to POINTs
#    and, pull out the second point, because we want the point on the 'roads' object,
#    which was supplied second to st_nearest_points()
my_linestring %>%
  st_cast('POINT') %>%
  .[2] %>%
  {. ->> closest_point}

closest_point



mapview(snapped_pts, col.regions="orange")+ mapview(bird_pts, col.regions="purple") +
  mapview(nearest_pts, col.regions="blue", cex=4) +
  mapview(flowlines_simple, lwd=0.4)

# Networksf ---------------------------------------------------------------


library(sfnetworks)

flowlines_net <- as_sfnetwork(flowlines_s)

# get nearest Nodes
nearest_nodes <- st_nearest_feature(bird_pts, flowlines_net)

# Snap geometries of POIs to the network
snapped_pts <- bird_pts  |>
  st_set_geometry(st_geometry(flowlines_net)[nearest_nodes])


# Use NN Approach ---------------------------------------------------------

nearest_coords <- st_nn(bird_pts, flowlines_simple, k = 1, returnDist = T)

# make a df
nearest_df <- tibble("comid"=unlist(nearest_coords$nn), dist_m=unlist(nearest_coords$dist), SiteID=bird_pts$SiteID)

# add sf
birds_dists2 <- left_join(bird_pts, nearest_df)

# trim flowline layer
flowline_birds <- flowlines[birds_dists2$comid,] |>
  select(comid, gnis_id:streamorde, pathlength:arbolatesu)

mapview(birds_dists2, zcol="dist_m") + mapview(flowline_birds)


# Use Snapping Approach ---------------------------------------------------

st_snap_points <- function(x, y, namevar, max_dist = 1000) {

  # this evaluates the length of the data
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)

  # this part:
  # 1. loops through every piece of data (every point)
  # 2. snaps a point to the nearest line geometries
  # 3. calculates the distance from point to line geometries
  # 4. retains only the shortest distances and generates a point at that intersection
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  # this part converts the data to a dataframe and adds a named column of your choice
  out_xy <- st_coordinates(out) %>% as.data.frame()
  out_xy <- out_xy %>%
    mutate({{namevar}} := x[[namevar]]) %>%
    st_as_sf(coords=c("X","Y"), crs=st_crs(x), remove=FALSE)

  return(out_xy)
}

# takes a while
snapped <- st_snap_points(bird_pts, flowlines_simple, namevar = "SiteID", 5000)

mapview(snapped) + mapview(flowlines_simple)


# Or THis -----------------------------------------------------------------

dist_for_edge <- st_geometry(obj = polygons_sf) %>%
  st_cast(to = 'MULTILINESTRING') %>%
  st_distance(y=points_sf)



# OPtion 900 --------------------------------------------------------------

st_snap_to_network = function(x, y) {
  # Function to find the nearest network point to a single input point.
  f = function(z) {
    # Convert sfg to sfc.
    z = sf::st_sfc(z, crs = sf::st_crs(x))
    # Run st_nearest_points.
    # This returns for each combination (z, y) a linestring from z to the
    # .. nearest point on y.
    np = sf::st_nearest_points(z, y)
    # Then, we get the np-line to the nearest feature y from z
    np[sf::st_nearest_feature(z, y),] ### LINE CHANGED HERE!
  }
  # Run f for all input points.
  geoms = do.call("c", lapply(sf::st_geometry(x), f))
  # Replace the geometries of x with their nearest network points.
  if (inherits(x, "sf")) sf::st_geometry(x) = geoms
  if (inherits(x, "sfc")) x = geoms
  x
}git
