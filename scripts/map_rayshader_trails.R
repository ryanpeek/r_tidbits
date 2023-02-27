# rayshader trail map

# and this: https://michaelpaulschramm.com/posts/2021-04-15-rayshade-precipitation/

# see this post: https://www.tylermw.com/adding-open-street-map-data-to-rayshader-maps-in-r/

library(rayshader)
library(raster)
#library(terra)
library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)


# Read in Data ------------------------------------------------------------

# using terra
clake_rast <- terra::rast("/Users/rapeek/Downloads/Crater_Lake_GeoTIFF/Crater_Lake.tif")
terra::crs(clake_rast, describe=TRUE)$name # 26910
writeLines(st_crs(clake_rast)$WktPretty)

# using raster
clake <- raster("/Users/rapeek/Downloads/Crater_Lake_GeoTIFF/Crater_Lake.tif")
clake_mat <- raster_to_matrix(clake)

# resize
clake_small <- resize_matrix(clake_mat,0.25)

# BaseMap -----------------------------------------------------------------

clake_small %>%
  height_shade() %>%
  plot_map()

## Plot: Spherical Aspect Shading ---------------
# add spherical aspect shading
clake_small %>%
  height_shade() %>%
  add_overlay(sphere_shade(clake_small,
                           texture = "desert",
                           zscale=4,
                           colorintensity = 5),
              alphalayer=0.5) %>%
  plot_map()

## Plot: Hillshade ------------------

clake_small %>%
  height_shade() %>%
  add_overlay(sphere_shade(clake_small,
                           texture = "desert",
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(clake_small,zscale = 6),0) %>%
  plot_map()

## Plot: Texture Shade ----------------------------

# better defines ridges and drainage networks
clake_small %>%
  height_shade() %>%
  add_overlay(sphere_shade(clake_small, texture = "desert",
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(clake_small,zscale=6), 0) %>%
  add_shadow(texture_shade(clake_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
  plot_map()


## Plot: Ambient Occlusion ------------------------
# darken valleys between steep ridges

clake_small %>%
  height_shade() %>%
  add_overlay(sphere_shade(clake_small, texture = "desert",
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(clake_small,zscale=6), 0) %>%
  add_shadow(ambient_shade(clake_small), 0) %>%
  add_shadow(texture_shade(clake_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
  plot_map()

# CHANGE EXTENT ----------------
## raster::bbox ----------------------------------------------------------

lat_range <- c(42.893921,42.961074)
lon_range <- c(-122.196672, -122.118120)
raster::crs(clake)

convert_coords = function(lat,long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  proj4string(data) = from
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}

(utm_bbox <- convert_coords(lat = lat_range, long=lon_range, to = raster::crs(clake)))

# now crop w zoomed (raster):
(extent_zoomed_rast <- raster::extent(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4]))

clake_zoom <- crop(clake, extent_zoomed_rast)
clake_zoom_mat <- raster_to_matrix(clake_zoom)

plot(clake_zoom)

## terra::bbox --------------

lat_range <- c(42.893921,42.961074)
lon_range <- c(-122.196672, -122.118120)

terra::crs(clake_rast, describe=TRUE)$name # 26910

# make bbox
pts <- data.frame(lon=lon_range, lat=lat_range) %>% st_as_sf(., coords=c("lon", "lat"), crs=4326)

# transform
pts <- st_transform(pts,crs = terra::crs(clake_rast))

(utm_bbox_terra <- st_bbox(pts)) # returns slightly diff order

extent_zoomed_terra <- terra::ext(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4])

clake_zoom_terra <- crop(clake_rast, extent_zoomed_terra)
plot(clake_zoom_terra) # quick check
clake_zoom_mat <- raster_to_matrix(clake_zoom_terra)

# Yay Plot Zoomed ----------------------------------------------------------------

# make final zoomed (this takes a sec)
base_map <- clake_zoom_mat %>%
  height_shade() %>%
  add_overlay(sphere_shade(clake_zoom_mat, texture = "desert", colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(clake_zoom_mat), 0) %>%
  add_shadow(ambient_shade(clake_zoom_mat),0) %>%
  add_shadow(texture_shade(clake_zoom_mat,detail=8/10,contrast=9,brightness = 11), 0.1)

plot_map(base_map)

# OSM ---------------------------------------------------------------------

osm_bbox <- c(lon_range[1],lat_range[1], lon_range[2],lat_range[2])

# highways
clake_lines <- opq(osm_bbox) %>%
  add_osm_feature("highway") %>%
  osmdata_sf()

# transform to highways
clake_highway <- st_transform(clake_lines$osm_lines, crs=crs(clake))

# plot
ggplot() +
  geom_sf(data=clake_highway,aes(color=osm_id)) +
  theme_void() +
  guides(color="none")+
  labs(subtitle = "Open Street Map `highway` attribute in Crater Lake")

# now overlay
base_map %>%
  add_overlay(generate_line_overlay(
    clake_highway,
    linewidth = 3, color="white",
    extent = extent_zoomed_rast,
    heightmap = clake_zoom_mat)) %>%
  plot_map()


## OSM Roads & Trails ----------------------------------------------------

# add trails an such
clake_trails <- clake_highway %>%
  filter(highway %in% c("path","bridleway"))

clake_footpaths <- clake_highway %>%
  filter(highway %in% c("footway"))

clake_roads <- clake_highway %>%
  filter(highway %in% c("unclassified", "secondary", "tertiary", "residential", "service"))

# check
ggplot() +
  geom_sf(data=clake_roads,aes(color=osm_id)) +
  geom_sf(data=clake_footpaths,aes(color=osm_id)) +
  geom_sf(data=clake_trails,aes(color=osm_id)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(subtitle = "Open Street Map `highway` attribute in Crater Lake")

# try with footpaths w shadows so they show up more
base_map %>%
  add_overlay(generate_line_overlay(
    clake_footpaths, extent = extent_zoomed_rast,
    linewidth = 10, color="black", heightmap = clake_zoom_mat)) %>%
  add_overlay(generate_line_overlay(
    clake_footpaths, extent = extent_zoomed_rast,
    linewidth = 6, color="white", heightmap = clake_zoom_mat)) %>%
  add_overlay(generate_line_overlay(
    clake_trails, extent = extent_zoomed_rast,
    linewidth = 8, color="black", lty=3, offset = c(2,-2), heightmap = clake_zoom_mat)) %>%
  add_overlay(generate_line_overlay(
    clake_trails,
    extent = extent_zoomed_rast,
    linewidth = 6, color="white", lty=3, heightmap = clake_zoom_mat)) %>%
  add_overlay(generate_line_overlay(
    clake_roads, extent = extent_zoomed_rast,
                                    linewidth = 8, color="black",
                                    heightmap = clake_zoom_mat)) %>%
  plot_map()

# save object for later
trails_layer <- generate_line_overlay(
  clake_footpaths, extent = extent_zoomed_rast,
  linewidth = 10, color="black", heightmap = clake_zoom_mat) %>%
  add_overlay(generate_line_overlay(
    clake_footpaths, extent = extent_zoomed_rast,
    linewidth = 6, color="white", heightmap = clake_zoom_mat)) %>%
  add_overlay(generate_line_overlay(
    clake_trails, extent = extent_zoomed_rast,
    linewidth = 8, color="black", lty=3, offset = c(2,-2), heightmap = clake_zoom_mat)) %>%
  add_overlay(generate_line_overlay(
    clake_trails,
    extent = extent_zoomed_rast,
    linewidth = 6, color="white", lty=3, heightmap = clake_zoom_mat)) %>%
  add_overlay(generate_line_overlay(
    clake_roads, extent = extent_zoomed_rast,
    linewidth = 8, color="black",
    heightmap = clake_zoom_mat))

## OSM Water ----------------------------

clake_water_lines <- opq(osm_bbox) %>%
  add_osm_feature("waterway") %>%
  osmdata_sf()
clake_water_lines

# streams
clake_streams <- st_transform(clake_water_lines$osm_lines,crs=crs(clake))

# generate overlay separately
stream_layer <- generate_line_overlay(
  clake_streams, extent = extent_zoomed_rast,
  linewidth = 4, color="steelblue1",
  heightmap = clake_zoom_mat)

# now plot and adjust
base_map %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>%
  add_overlay(trails_layer) %>%
  plot_map()

## OSM Parking Structures ------------

clake_parking <- opq(osm_bbox) %>%
  add_osm_feature("parking") %>%
  osmdata_sf()

clake_building <- opq(osm_bbox) %>%
  add_osm_feature("building") %>%
  osmdata_sf()

clake_tourism <- opq(osm_bbox) %>%
  add_osm_feature("tourism") %>%
  osmdata_sf()

clake_parking_poly <- st_transform(clake_parking$osm_polygons,crs=crs(clake))
clake_building_poly <- st_transform(clake_building$osm_polygons,crs=crs(clake))
clake_tourism_poly <- st_transform(clake_tourism$osm_polygons,crs=crs(clake))

clake_sites_poly <- clake_tourism_poly %>%
  filter(!is.na(tourism))

polygon_layer <- generate_polygon_overlay(
  clake_parking_poly, extent = extent_zoomed_rast,
  heightmap = clake_zoom_mat, palette="grey30") %>%
  add_overlay(generate_polygon_overlay(
    clake_building_poly, extent = extent_zoomed_rast,
    heightmap = clake_zoom_mat, palette="yellow")) %>%
  add_overlay(generate_polygon_overlay(
    clake_sites_poly, extent = extent_zoomed_rast,
    heightmap = clake_zoom_mat, palette="darkgreen"), alphalayer = 0.6)

base_map %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>%
  add_overlay(trails_layer) %>%
  add_overlay(polygon_layer) %>%
  plot_map()

## OSM Labels -------------------

clake_tourism_points <- st_transform(clake_tourism$osm_points,crs=crs(clake))

clake_viewpoint <- clake_tourism_points %>%
  filter(tourism == "viewpoint")

##Set label font
par(family = "Roboto Condensed")

base_map %>%
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>%
  add_overlay(trails_layer) %>%
  add_overlay(generate_label_overlay(
    clake_viewpoint, extent = extent_zoomed_rast,
    text_size = 4, point_size = 3,point_color = "gold",
    color="white",seed=1,
    halo_color = "gray20",halo_expand = 5,
    halo_blur = 20, halo_alpha = 0.8,
    heightmap = clake_zoom_mat, data_label_column = "name")) %>%
  # add a title
  plot_map(title_text = "Crater Lake National Park, OR",
           title_offset = c(15,15),
           title_bar_color = "grey5", title_font = "Atkinson Hyperlegible",title_size = 90,title_style = "italic",
           title_color = "white", title_bar_alpha = 1)


# Render 3d ---------------------------------------------------------------

base_map %>%
  add_overlay(polygon_layer) %>%
  add_overlay(stream_layer, alphalayer = 0.8) %>%
  add_overlay(trails_layer) %>%
  plot_3d(clake_zoom_mat, windowsize=c(1200,800))
render_camera(theta=70,  phi=30, zoom=0.4,  fov=60)
render_snapshot()
