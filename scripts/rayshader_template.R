# rayshader template

# Rayshader basic workflow
# By Tobias Stalder
# October 2022
# Creative Commons licence (Free to use - Reference Author in Code by Publication)


# libraries ---------------------------------------------------------------

memory.limit(size=4000000000) #set memory limit

library(tidyverse) #wrangling and ggplot
library(sf) #sf features for polygons etc
library(rayshader) #3d rendering
library(here) #working directory management
library(raster) #raster wrangling
library(rgdal) #gis engine
library(rgl) #3d rendering engine
library(rayrender) #3d rendering



# #data

# geotiff of elevation for transect (pre-processed in qgis for crs reasons)
elev_img <- raster::raster(paste0(here(), "\\data\\DHM25_MM_ASCII_GRID\\ASCII_GRID_1part\\dhm25_rect_WGS84.tif"))

#geotiff of elevation for transec in 200m resolution (lower res than elev_img)
elev_img_200 <- raster::raster(paste0(here(), "\\data\\DHM25_MM_ASCII_GRID\\ASCII_GRID_1part\\dhm200_transect_wgs84.tif"))


#transform geotiff to matrix
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000),
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)


dim(elev_matrix) #check matrix dimensions

#calculate rayshade and ambient shadow for transect
ambmat <- ambient_shade(elev_matrix, zscale = 10)
raymat <- ray_shade(elev_matrix, zscale = 10, lambert = TRUE)

#check if dimensions of shadow layers and the elevation matrix match!
dim(elev_matrix)
dim(ambmat)
dim(raymat)


#plot transect 2d for check
elev_matrix %>%
  sphere_shade(texture=create_texture("#6D597A","#355070",
                                               "#B56576","#E56B6F","#EAAC8B"),
                                               sunangle = 45) %>%
  add_shadow(raymat, max_darken = 0.2) %>%
  add_shadow(ambmat, max_darken = 0.2) %>%
  plot_map()

# plot 3D with rayshader
elev_matrix %>%
  sphere_shade(texture=create_texture("#6D597A","#355070",
                                               "#B56576","#E56B6F","#EAAC8B"),
                                               sunangle = 45) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.3) %>%
  plot_3d(elev_matrix, zscale =10, windowsize = c(2000, 600), zoom = 0.5)
render_snapshot(filename = paste0(here(),"/terrain_model.png")) #render snapshot (rgl window was enlarged since the output quality of the
