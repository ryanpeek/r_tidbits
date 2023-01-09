library(sf)
library(nhdplusTools)
library(mapview)
mapviewOptions(fgb=FALSE)

start_point <- sf::st_as_sf(data.frame(x = -120.851787, y = 39.492949),
                            coords = c("x", "y"), crs = 4326)
mapview(start_point)

plot_nhdplus(start_point)


library(elevatr)
elevatr::get_aws_points(start_point, z = 10, units = "feet")
