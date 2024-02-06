# Get MODIS data

# see terra tutorial: https://rspatial.org/modis/2-download.html

# remotes::install_github("rspatial/luna")
# library(luna)

# remotes::install_github("ropensci/MODIStsp")
# tutorial https://docs.ropensci.org/MODIStsp/articles/noninteractive_execution.html
library(MODIStsp)
library(terra)
library(tidyverse)
library(viridis)
library(glue)
library(sf)


# distrib modeling:
# https://ebird.github.io/ebird-best-practices/envvar.html

# Set Proxy ---------------------------------------------------------------

library(httr)
set_config(
  use_proxy(url = "http://CDFWProxy.ad.dfg.ca.gov", port = 8080, auth = "ntlm", username = "", password = ""))


# Check Layers ------------------------------------------------------------

MODIStsp_get_prodlayers("M*D13Q1")
MODIStsp_get_prodnames()

# Download ----------------------------------------------------------------
#
# MODIStsp(gui             = FALSE,
#          out_folder      = "$tempdir",
#          selprod         = "Vegetation_Indexes_16Days_1Km (M*D13A2)",
#          bandsel         = c("EVI", "NDVI"),
#          quality_bandsel = "QA_usef",
#          indexes_bandsel = "SR",
#          user            = "mstp_test" ,
#          password        = "MSTP_test_01",
#          start_date      = "2020.06.01",
#          end_date        = "2020.06.15",
#          verbose         = FALSE)
#
# out_fold <- file.path(tempdir(), "MODIStsp/VI_16Days_1Km_v61/")
# list.files(out_fold)
#
# list.files(file.path(out_fold ,"EVI"))


# Get an AOI --------------------------------------------------------------

#ca_county <- tigris::counties("CA")
# save out
#write_rds(ca_county, "data_raw/ca_counties.rds")
ca_county <- read_rds("data_raw/ca_counties.rds")
aoi <- ca_county |> filter(NAME=="El Dorado")

plot(ca_county$geometry, col="light gray")
plot(aoi$geometry, col=alpha("red",0.2), lwd=2, border="maroon", add=T)

# save it out as a shpfile
# st_write(aoi, "data_raw/el_dorado_cnty.shp")

# Download 2 --------------------------------------------------------------

# make dir
fs::dir_create("data_raw/MODIS")

# this downloads data and makes json that can be reused and makes rda file
MODIStsp(gui             = FALSE,
         out_folder      = 'data_raw/MODIS/',
         out_folder_mod  = 'data_raw/MODIS/',
         selprod         = 'LandCover_Type_Yearly_500m (MCD12Q1)',
         bandsel         = 'LC1',
         sensor          = 'Terra',
         user            = 'ryan.peek' , # your username for NASA http server
         password        = Sys.getenv("NASA_PW"),  # your password for NASA http server
         start_date      = '2001.01.01',
         end_date        = '2023.12.31',
         verbose         = TRUE,
         spafile = "data_raw/el_dorado_cnty.shp",
         spatmeth        = 'file',
         out_format      = 'GTiff',
         compress        = 'LZW',
         #out_projsel     = 'User Defined',
         #output_proj     = '+proj=laea +lon_0=-73.125 +lat_0=0 +datum=WGS84 +units=m +no_defs',
         delete_hdf      = TRUE,
         parallel        = TRUE
)


# read in data
terr_rast <- terra::rast(here::here("data_raw/MODIS/el_dorado_cnty/LandCover_Type_Yearly_500m_v61/LC1/MCD12Q1_LC1_2022_001.tif"))

# Transforming data
terr_rast_proj <- terra::project(terr_rast, "epsg:4269")

# Cropping data
terr_rast_crop <- terra::crop(terr_rast_proj, aoi, mask=TRUE)

# Converting the raster object into a dataframe and converting the IGBP classification into a factor
terr_df <- as.data.frame(terr_rast_crop, xy = TRUE, na.rm = TRUE)  |>
  rename("landcover" = 3) |>
  mutate(landcover = as.factor(round(landcover)))
rownames(terr_df) <- c()

# check classifications levels for NA
# https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf

# Renaming classification levels
levels(terr_df$landcover) <- c( "Evergreen needleleaf forests",
                                           "Evergreen broadleaf forests",
                                           "Deciduous needleleaf forests",
                                           "Deciduous broadleaf forests",
                                           "Mixed forests",
                                           "Closed shrublands",
                                           "Open shrublands",
                                           "Woody savannas",
                                           "Savannas",
                                           "Grasslands",
                                           "Permanent wetlands",
                                           "Croplands",
                                           "Urban and built-up lands",
                                           "Cropland/natural vegetation mosaics",
                                           "Permanent snow and ice",
                                           "Barren",
                                           "Water bodies")
# Visualising using ggplot2
(p2 <- ggplot() +
  geom_raster(data = terr_df,
              aes(x = x, y = y, fill = landcover)) +
  geom_sf(data = aoi, inherit.aes = FALSE, fill = NA) +
  cols4all::scale_fill_discrete_c4a_cat(name = "Land Cover Type",
                                        palette = "tableau.20") +
  labs(title = "Land Cover classification in El Dorado County",
       subtitle = "2022",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal())



# tutorials -----------------


# https://flograttarola.com/post/modis-downloads/
