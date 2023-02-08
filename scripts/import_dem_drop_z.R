# delta dem

# this is an example of when you can't read a file in because of a weird Z dim error


# Libraries ---------------------------------------------------------------
library(sf)
library(terra)
library(dplyr)
library(wateRshedTools)

# this fails with this error
#dem_src <- wateRshedTools::get_shp_zip("~/Downloads/TEMP_data/baydelta_dem/data_source_dem_20201209.zip", web = FALSE)

# Error in CPL_get_z_range(obj, 3) : z error - expecting three columns;


# Read with Terra ---------------------------------------------------------
# terra will ignore the z

# use terra
dem_src <- terra::vect("~/Downloads/TEMP_data/baydelta_dem/data_source_dem_20201209/data_source_dem_20201209.shp")
plot(dem_src)

#then write out and over the original one

# write out with terra:
writeVector(x = dem_src, filename = "/Users/rapeek/Downloads/TEMP_data/baydelta_dem/data_source_dem_20201209/data_source_dem_20201209.shp", overwrite=TRUE)


# Overwrite with sf gdal utils --------------------------------------------

# write out with sf:
gdal_utils(util="vectortranslate",
           source="~/Downloads/TEMP_data/baydelta_dem/data_source_dem_20201209/data_source_dem_20201209.shp",
           destination = "~/Downloads/TEMP_data/baydelta_dem/data_source_dem_20201209/data_source_dem_20201209.shp",
           options = c("-dim","XY"))


# Read back in ------------------------------------------------------------


# read
st_read("~/Downloads/TEMP_data/baydelta_dem/data_source_dem_20201209/data_source_dem_20201209.shp")
