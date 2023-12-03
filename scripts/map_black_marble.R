# black marble NASA!

# https://blackmarble.gsfc.nasa.gov/
# https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/VNP46A2/2020/
#https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/VNP46A2/2020/080/

library(terra)
library(glue)
library(dplyr)
library(sf)

# Get Shapefile -----------------------------------------------------------

# download.file("https://blackmarble.gsfc.nasa.gov/tools/BlackMarbleTiles.zip", destfile = "data/BlackMarbleTiles.zip")
# unzip("data/BlackMarbleTiles.zip", exdir = "data")

# Get Data ----------------------------------------------------------------

# need to be logged in or this doesn't work manually
# u <- "https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/VNP46A2/2020/122/VNP46A2.A2020122.h05v16.001.2021068194747.h5"
#
# download.file(u, destfile = glue("data/{basename(u)}"))


# Use SF to View Grid of Interest -----------------------------------------

library(mapview)
mapviewOptions(fgb=FALSE)
bm_grid <- st_read("data_raw/BlackMarbleTiles/BlackMarbleTiles.shp") #%>%
  #st_transform(3310)
st_crs(bm_grid)
mapview::mapview(bm_grid, zcol="TileID", legend=FALSE)

# india: h24v06:h26v06, h25v07, h26v07

# Get Extents -------------------------------------------------------------

# using terra to get extent
v <- vect("data_raw/BlackMarbleTiles/BlackMarbleTiles.shp")
tile <- v[v$TileID == "h06v05", ] # sCA and AZ
tile <- v[v$TileID == "h05v05", ] # nCA
tile <- v[v$TileID == "h17v03", ] # Ireland/UK


# Read Data ---------------------------------------------------------------
# IRL
r1 <- terra::rast('data_raw//VNP46A2.A2020080.h17v03.001.2021062011031.h5')
r1 <- terra::rast('data_raw//VNP46A2.A2022030.h17v03.001.2022047162659.h5')

# CA
r1 <- terra::rast('data_raw//VNP46A2.A2022030.h05v05.001.2022047154547.h5')
r1 <- terra::rast('data/VNP46A2.A2020080.h05v05.001.2021062204428.h5')

NAflag(r1) <- 65535
ext(r1) <- ext(tile)

r <- r1$`DNB_BRDF-Corrected_NTL`
r <- r1$`Gap_Filled_DNB_BRDF-Corrected_NTL`
#r <- r1$QF_Cloud_Mask
#r <- r1$Snow_Flag
plot(r)
