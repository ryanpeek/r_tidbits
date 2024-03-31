# centerline

# devtools::install_github("atsyplenkov/centerline")
# https://github.com/atsyplenkov/centerline
library(centerline)
library(terra)

polygon <-
  terra::vect(
    system.file(
      "extdata/example.gpkg", package = "centerline"
    ),
    layer = "polygon"
  )

# Find POI's skeleton
pol_skeleton <-
  cnt_skeleton(polygon, keep = 1)

plot(pol_skeleton,
     col = "dodgerblue3")
plot(polygon,
     border = "grey20",
     main = "Original",
     add = T)

# Simplified POI's skeleton
pol_skeleton_simplify <-
  cnt_skeleton(polygon, keep = .1)

plot(pol_skeleton_simplify,
     col = "dodgerblue3")
plot(polygon,
     border = "grey20",
     main = "Simplified",
     add = T)

# Densified POI's skeleton
pol_skeleton_densify <-
  cnt_skeleton(polygon, keep = 1.5)

# plot
plot(pol_skeleton_densify,
     col = "dodgerblue3")
plot(polygon,
     border = "grey20",
     main = "Densified",
     add = T)


# Points -----------

# Load points data
points <-
  terra::vect(
    system.file(
      "extdata/example.gpkg", package = "centerline"
    ),
    layer = "polygon_points"
  )

# Connect points
# For original skeleton
pol_path <-
  cnt_path(
    skeleton = pol_skeleton,
    start_point = terra::subset(points, points$type == "start"),
    end_point = terra::subset(points, points$type != "start")
  )

# For simplified skeleton
pol_path_simplify <-
  cnt_path(
    skeleton = pol_skeleton_simplify,
    start_point = terra::subset(points, points$type == "start"),
    end_point = terra::subset(points, points$type != "start")
  )

# For densified skeleton
pol_path_dens <-
  cnt_path(
    skeleton = pol_skeleton_densify,
    start_point = terra::subset(points, points$type == "start"),
    end_point = terra::subset(points, points$type != "start")
  )


# Original
plot(polygon, border = "grey20",
     main = paste0("Original path (L = ",
                   round(terra::perim(pol_path[[1]]), 2), " m)"))
plot(pol_path[[1]], lwd = 3, add = T)
plot(points[1, ], col = "coral2",  add = T)
plot(points[2, ], col = "green4",  add = T)

# Simplified
plot(polygon, border = "grey20",
     main = paste0("Simplified path (L = ",
                   round(terra::perim(pol_path_simplify[[1]]), 2), " m)"))
plot(pol_path_simplify[[1]], lwd = 3, add = T)
plot(points[1, ], col = "coral2",  add = T)
plot(points[2, ], col = "green4",  add = T)

# Densified
plot(polygon, border = "grey20",
     main = paste0("Densified path (L = ",
                   round(terra::perim(pol_path_dens[[1]]), 2), " m)"))
plot(pol_path_dens[[1]], lwd = 3, add = T)
plot(points[1, ], col = "coral2",  add = T)
plot(points[2, ], col = "green4",  add = T)


# Using SF ----------------------------------------------------------------


library(sf)
library(geomtextpath)
library(smoothr)
library(tidyverse)

lake <-
  st_read(
    system.file("extdata/example.gpkg", package = "centerline"),
    layer = "lake",
    quiet = T
  ) |>
  st_cast("POLYGON")

lake_centerline <-
  cnt_path_guess(lake, keep = 1)

plot(lake$geom)
plot(lake_centerline, add=T, col="blue")

# smoothed
lake_centerline_s <-
  lake_centerline |>
  st_union() |>
  st_line_merge() |>
  st_simplify(dTolerance = 150) |>
  smooth("chaikin")

# double
cnt2 <-
  rbind(
    st_as_sf(lake_centerline_s),
    st_as_sf(lake_centerline_s)
  )

cnt2$lc <- c("black", NA_character_)
cnt2$ll <- c("", lake$name)

# plot
ggplot() +
  geom_sf(
    data = lake, fill = "#c8e8f1", color = NA) +
  geom_textsf(
    data = cnt2, aes( linecolor = lc, label = ll),
    color = "#458894", size = 8, family = "Liberation Mono") +
  scale_color_identity() +
  facet_wrap(~lc) + theme_void() +
  theme(strip.text = element_blank())
