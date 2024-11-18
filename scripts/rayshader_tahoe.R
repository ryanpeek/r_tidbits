# using rayshader to pull tahoe bathymetry

library(elevatr)
library(sf)
sf_use_s2(FALSE)
library(rayshader)
library(terra)
library(rnaturalearth)
library(tigris)
library(dplyr)
library(mapview)

# Data --------------------------------------------------------------------

ca <- counties(state = c("CA", "NV")) %>%
  st_transform(., crs = st_crs(4326))

# get tahoe with RNatural Earth
l <- rnaturalearth::ne_download(type = "lakes", category = "physical", scale = "medium", returnclass = "sf")  %>%
  filter(grepl("Tahoe", name))

# get better lake outline
library(nhdplusTools)
nhd_l <- get_waterbodies(l)
plot(nhd_l$geometry)

# get counties that intersect w tahoe
ca_tahoe <- ca[nhd_l,]
# get everything except for tahoe
counties_slim <- sf::st_difference(ca_tahoe, nhd_l)

# See Bathybase -----------------------------------------------------------

# tahoe bathymetry rasters

# http://www.bathybase.org
tahbath <- "http://www.bathybase.org/Data/1-99/2/bathy.tiff"
t_bath <- rast(tahbath)

# plot
plot(t_bath)
plot(st_transform(nhd_l$geometry, 32610), add=TRUE, border.col="steelblue")

tbath_proj <- project(t_bath,"epsg:4326")
plot(tbath_proj)
plot(nhd_l$geometry, add=TRUE, border.col="steelblue")

# RayShader ---------------------------------------------------------------

# Query for elevation data masked by our 'location' specified
tahoe_elev_trim <- get_elev_raster(nhd_l, z = 10, clip = "location")
tahoe_elev <- get_elev_raster(nhd_l, z = 10)
# plot
plot(tahoe_elev)
plot(nhd_l$geometry, add=TRUE, border.col="steelblue")


##  Convert to Matrix ------------------------------------------------------
t_bath_r <- raster::raster(t_bath)
mat <- raster_to_matrix(t_bath_r)
# convert to negative
mat <- mat*-1
## Set Rayshader params ----------------------------------------------------

library(colorspace)
library(NatParksPalettes)

pal <- "banff_arches"

c1 <- natparks.pals("Banff")
c2 <- natparks.pals("Arches")

colors <- c(rev(c1[2:5]), c2[2:5])
swatchplot(colors)

textures <- grDevices::colorRampPalette(colors)(256)

swatchplot(textures)

# set shadow to 500 feet below minimum value in DEM
shadow_depth <- min(mat, na.rm = TRUE) - 500

# Dynamically set window height and width based on object size
w <- nrow(mat)
h <- ncol(mat)

# Scale the dimensions so we can use them as multipliers
wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

# Limit ratio so that the shorter side is at least .75 of longer side
if (min(c(wr, hr)) < .75) {
  if (wr < .75) {
    wr <- .75
  } else {
    hr <- .75
  }
}

# close prev window
try(rgl::rgl.close())

# Plot data with `plot_3d()`
mat %>%
  height_shade(texture = textures) %>%
  plot_3d(heightmap = mat,
          #baseshape = "circle",
          windowsize = c(800*wr,800*hr),
          solid = FALSE,
          zscale = 2,
          phi = 60,
          theta = 45,
          shadowdepth = shadow_depth,
          zoom = 2)

save_obj("data_out/tahoe.obj")

# Close the window when you're done
rgl::rgl.close()


# High Res ----------------------------------------------------------------
rayrender::obj_model("data_out/tahoe.obj")
render_highquality(
  filename = "figs/tahoe_highres.png",
  parallel = TRUE,
  samples = 300,
  light = FALSE,
  interactive = FALSE,
  environment_light = "data_raw/phalzer_forest_01_4k.hdr",
  intensity_env = 1.5,
  rotate_env = 180,
  width = round(1000 * wr), # was 6000
  height = round(1000 * hr) # was 6000
)

### Don't UNDERSTAND!!!

#Error in rayrender::obj_model(cache_filename, x = -bbox_center[1], y = -bbox_center[2],  :
#                                unused argument (texture = TRUE)

# Annotate ----------------------------------------------------------------

# Load magick library, which provides R interface with ImageMagick
library(magick)

# Read in image, save to `img` object
img <- image_read("figs/tahoe_highres.png")

# Set text color
text_color <- colors[1]

# Title
img_ <- image_annotate(img, "Water:", font = "Cinzel Decorative",
                       color = colors[1], size = 125, gravity = "north",
                       location = "+0+200")
# Subtitle
img_ <- image_annotate(img_, "Tahoe Bathymetry", weight = 700,
                       font = "Cinzel Decorative", location = "+0+400",
                       color = text_color, size = 200, gravity = "north")
