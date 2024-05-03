

# Libraries ---------------------------------------------------------------

library(sf)
library(nhdplusTools)
library(mapview)
library(rmapshaper)
library(tidyverse)
mapviewOptions(fgb=FALSE)

start_point <- sf::st_as_sf(
  data.frame(x = -122.42048, y = 38.83013),
  coords = c("x", "y"), crs = 4326)
mapview(start_point)

dat_temp <- plot_nhdplus(start_point,actually_plot = FALSE)

plot(dat_temp$basin$geometry)
plot(dat_temp$flowline$geometry, add=TRUE, col = "steelblue")

# get huc12
h12 <- get_huc(AOI = dat_temp$basin$geometry, type="huc12")

plot(h12$geometry)
plot(dat_temp$basin$geometry, add=TRUE, col=alpha("cyan4", 0.2))
plot(dat_temp$flowline$geometry, add=TRUE, col = "steelblue", lwd=2)
plot(start_point, pch=21, add=TRUE, bg = "orange")

# Generate Random Points --------------------------------------------------

# now we have huc12, make points
pt_ran <- sf::st_sample(x = h12, size = c(rep(40, nrow(h12))), type="random") |> st_as_sf()
pt_reg <- sf::st_sample(x = h12, size = c(rep(40, nrow(h12))), type="regular") |> st_as_sf()
pt_hex <- sf::st_sample(x = h12, size = c(rep(40, nrow(h12))), type="hexagonal") |> st_as_sf()

plot(h12$geometry, border="blue")
plot(pt_reg, add=TRUE, pch=21, col="gray")
plot(pt_hex, add=TRUE, pch=22, col="orange")
plot(pt_ran, add=TRUE, pch=8, col=alpha("red",.4), cex=0.5)


# FVEG --------------------------------------------------------------------

# https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=

# try just coastal, see mapping zones here: https://www.fs.usda.gov/detail/r5/landmanagement/resourcemanagement/?cid=stelprdb5347192
st_layers("/Users/rapeek/Downloads/S_USA.EVMid_R05_NorCoastMid.gdb")
veg_nc_mid <- st_read("/Users/rapeek/Downloads/S_USA.EVMid_R05_NorCoastMid.gdb", "EVMid_R05_NorCoastMid") |>
  st_transform(3310) |>
  select(SOURCE_DATE_YEAR, CALVEGZONE:COVERTYPE, geometry=SHAPE)

veg_bb <- st_bbox(veg_nc_mid) |> st_as_sfc() |> st_sf()
plot(veg_bb)
plot(h12$geometry, border="blue", add=T)

# crop
h12_veg <- st_intersection(h12, veg_nc_mid)

# make a factor for better levels
h12_veg$COVERTYPE <- factor(h12_veg$COVERTYPE)

plot(h12$geometry, border="blue")
plot(h12_veg$geometry, add=TRUE, border="brown2")

# Spatial Sampling --------------------------------------------------------

# basic by stratum
table(h12_veg$COVERTYPE, useNA = "ifany")

# calc area of each
h12_veg |>
  group_by(COVERTYPE) |>
  summarize(tot_area=sum(st_area(geometry))) |>
  mutate(tot_area_km2 = units::set_units(tot_area, "km^2"), .before=tot_area)

# try this
set.seed(123)

# Function to generate random points within each group
generate_random_points <- function(group_df, no_pts) {
  st_sample(group_df$geometry, size = no_pts, type = "hexagonal") %>%
    st_as_sf() |> rename(geometry=x)
}

# Generate random points within each group
pts_strat <- h12_veg  |>
  group_by(COVERTYPE)  |>
  group_split() |>
  map(generate_random_points, 5) %>%
  bind_rows()

pts_veg <- st_intersection(pts_strat, h12_veg)
table(pts_veg$COVERTYPE)

# plot
plot(h12$geometry, border="blue")
plot(h12_veg["COVERTYPE"], add=TRUE, )
plot(pt_reg, add=TRUE, pch=21, bg="white")
plot(pts_veg["COVERTYPE"], add=TRUE, pch=16, cex=3)

library(colorspace)
pal <- "hiroshige"
colors <- MetBrewer::met.brewer("Hiroshige", length(unique(h12_veg$COVERTYPE)))
#swatchplot(colors)

mapview(h12, color="blue", alpha.regions=0) +
  mapview(h12_veg, zcol="COVERTYPE", col.regions=terrain.colors(8, rev = T)) +
  mapview(pts_veg, zcol="COVERTYPE", cex=4, lwd=2, col.regions=terrain.colors(8, rev = T))



# Now Buffer --------------------------------------------------------------

min_distance <- units::set_units(250, "m")
max_distance <- units::set_units(700, "m")

# distance matrix of all pts
pts_dists <- st_distance(pts_veg, pts_veg)
pts_valid <- which(pts_dists >= min_distance & pts_dists <= max_distance, arr.ind = TRUE)

# filter to these points only
pts_filtered <- pts_veg[unique(c(pts_valid[, "row"], pts_valid[, "col"])), ]

# preview?
m1 <- mapview(pts_veg, zcol="COVERTYPE", cex=2)+
mapview(pts_filtered, zcol="COVERTYPE")
library(leaflet)
m1@map |> addMeasure(primaryLengthUnit = "meters")


pair_pts <- st_within(pts_veg,buffered_points)
# this works but returns a matrix/array
min_max_pts <- st_distance(buffered_points, pts_veg) >= min_distance & st_distance(buffered_points, pts_veg) <= max_distance

# Find pairs of points that are within the desired distance range
selected_pairs <- #st_within(pts_veg, buffered_points) |>
  st_filter(pts_veg, st_distance(buffered_points, pts_veg) >= min_distance & st_distance(buffered_points, pts_veg) <= max_distance)


# Filter unique pairs to avoid duplication
unique_pairs <- st_unique(selected_pairs)

# Sample four points from each unique pair
selected_points <- unique_pairs %>%
  group_by(index) %>%
  slice_sample(n = 4, replace = FALSE)
