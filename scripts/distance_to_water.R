# distance to water


# Libraries ---------------------------------------------------------------

library(sf)
library(nhdplusTools)
library(tidyverse)
library(nngeo)
library(rmapshaper)

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

# Simplify ----------------------------------------------------------------

crs <- 3310

flowlines <- flowlines |>
  st_transform(crs) |>
  rmapshaper::ms_simplify(keep=0.1) |>
  st_intersection(bbox_poly)

water_bodies <-
  st_transform(water_bodies, crs) |>
  st_buffer(0) |>
  rmapshaper::ms_simplify(keep = 0.05) |>
  st_intersection(bbox)

# Use NN Approach ---------------------------------------------------------

nearest_coords <- st_nn(bird_pts, flowlines_sf, k = 1, returnDist = T)

# make a df
nearest_df <- tibble("comid"=unlist(nearest_coords$nn), dist_m=unlist(nearest_coords$dist), SiteID=search$SiteID)

# add sf
birds_dists <- left_join(bird_pts, nearest_df)

mapview(birds_dists)

# Approach 1 --------------------------------------------------------------

#add the coordinates for the points in SF dataframe A and B --------------
a_coord <- st_coordinates(a)
a <- cbind(a, a_coord)

b_coord <- st_coordinates(b)
b <- cbind(b, b_coord)


#get closest feature in B to A -----------------------------------------
A_B <- a %>%
  st_join(b %>%
            select(B_ID, X, Y) %>%
            rename(B_X = X, B_Y = Y), join = st_nearest_feature)



#create a WKT from the coords of A and closest feature in B --------------
A_B$line_wkt <- paste('linestring(',A_B$X,A_B$Y,',',A_B$B_X,A_B$B_Y,')')


#Convert WKT into Geom--------------------------------------
A_B <- A_B %>%
  st_drop_geometry() %>%
  st_as_sf( wkt = 'line_wkt ', crs= 4326)


#Get the length (distance) of each line ----------------------------------
A_B$length <- as.numeric(st_length(A_B) )


#Join results with original A --------------------------------------------
a <- a %>%
  left_join(A_B %>%
              st_drop_geometry() %>%
              select(A_ID, B_ID, length), by = 'A_ID')
