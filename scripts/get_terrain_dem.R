# see here: https://github.com/matthew-law/30DayMapChallenge2022/blob/main/raster/raster.R

library(sf)
library(rayshader)
library(leaflet)
library(terrainr)
library(terra)

# this is the centre of the map
my_site <- data.frame(x = -121.22905, y = 43.72195) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# 8km squared box around that centre
my_bbox <- set_bbox_side_length(my_site, 8000)

mapview::mapview(my_site) +
  mapview::mapview(st_as_sf(my_bbox))

# reproject to EPSG:3857
my_bbox_3310 <- st_transform(my_bbox, 3310)

# get elevation tiles with terrainr::get_tiles
resolution <- 4
output_tiles_2m <- get_tiles(my_bbox_3310,
                             services = "elevation",
                             resolution = resolution,
                             projected = TRUE,
                             bboxSR = 3310)

# convert to raster/matrix
elevation <- output_tiles_2m[[1]] %>%
  merge_rasters() %>%
  raster_to_matrix()

rivs <- nhdplusTools::get_nhdplus(my_bbox_3310)
lks <- nhdplusTools::get_waterbodies(my_bbox_3310)

plot(my_bbox_3310, lwd=2, border="gray")
plot(rivs$geometry, add=T, col="blue")
plot(lks$geometry, add=TRUE, col="steelblue")

# colours for the sphere_shade texture
texture = create_texture("#f5dfca","#63372c","#dfa283","#195f67","#c9e3c5",
                         cornercolors = c("#ffc500", "#387642", "#d27441","#296176"))
plot_map(texture) # nice way to have a look at them

# get texture with low sun altitude
system.time(
  raymat <- ray_shade(elevation, zscale = resolution/5,
                      multicore = TRUE,
                      # changre sun alt
                      anglebreaks = seq(35, 45, by = 0.5))
)

# have a look at the layer it produces
raymat %>% plot_map()

# transform to matrix
raymat2 <- raymat # to get a matrix the right size

# but this worked and made sense to me
raymat2[] <- vapply(raymat, function(x) ((1.3*x-0.3)^3 - (1.3*x-0.5)^2), numeric(1))

# rescaling (so the values are between 0 and 1)
# done automatically later in add_shadow()
raymat2 %>% scales::rescale() %>% plot_map()

river <- generate_polygon_overlay(geometry = lks,
                               extent = raster::extent(st_bbox(my_bbox_3310)),
                               heightmap = elevation,
                               linewidth = .4, linecolor = "cyan4")

elevation %>%
  sphere_shade(texture, sunangle = 0, colorintensity = 5) %>%
  add_overlay(river, alphalayer = 0.8) %>%
  add_shadow(raymat2, max_darken = 0.2) %>%
  plot_map(maxpixels = length(elevation))

