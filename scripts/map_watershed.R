# get and plot watershed quickly


# Libraries ---------------------------------------------------------------

library(here)
library(tidyverse)
library(sf)
library(glue)
library(tigris)
library(nhdplusTools)
library(geoarrow)
library(rmapshaper)
library(mapview)
mapviewOptions(fgb=FALSE)
source("scripts/f_get_fonts.R")

# Get State/Counties ------------------------------------------------------

ca <- tigris::states(progress_bar=FALSE) %>% filter(NAME=="California")
ca_cnty <- ca_cntys <- tigris::counties("CA", progress_bar=FALSE)

# Get HUC Watersheds ------------------------------------------------------

# can specify any given option for huc8, huc10, etc
# specify CA boundary
# huc8 <- nhdplusTools::get_huc(ca, type = "huc08") # this takes a minute or two
# huc8 <- st_cast(huc8, "MULTIPOLYGON") # fix geometry
# save out
# write_geoparquet(huc8, here("data_raw/nhd_huc08.parquet"))

# Watershed ---------------------------------------------------------------
# load
h8 <- read_geoparquet_sf(here("data_raw/nhd_huc08.parquet"))

# pull out a single watershed
# mapview::mapview(h8) # can view with mapview
watershed <- "San Francisco Bay"
h8_sel <- h8 %>% filter(grepl(watershed, name))
plot(h8_sel$geometry)
h8_sel_mrg <- rmapshaper::ms_dissolve(h8_sel)
plot(h8_sel_mrg$geometry)

# get water data
ca_water <- tigris::area_water("CA", tigris::list_counties("CA")$county)

# now crop by watershed
st_crs(ca_water) ==st_crs(h8_sel_mrg)
ca_water_sel <- ca_water[h8_sel_mrg,] # select via spatial join
ca_water_sel2 <- st_intersection(ca_water_sel, h8_sel_mrg)

# Now Get Data ------------------------------------------------------------

# pull mainstem rivers and lakes for watershed
shed_wb <- nhdplusTools::get_waterbodies(h8_sel_mrg) # water bodies

# get flowlines
shed_rivs <- get_nhdplus(h8_sel_mrg)

# Base Map ----------------------------------------------------------------

# set fonts depending on system:
f_get_fonts(fnt_pr = 1)

# quick map
plot(h8_sel$geometry, border = "gray50", lty=2)
plot(shed_rivs$geometry, col="steelblue4", lwd=shed_rivs$streamorde/4, add=TRUE)
plot(ca_water_sel2$geometry, border="cyan4", col="cyan4", add=TRUE)
plot(shed_wb$geometry, border="steelblue2", col=alpha("steelblue2",0.9), add=TRUE)
plot(h8_sel_mrg$geometry, border="gray40", lwd=3, add=TRUE)
title(main = glue("{watershed} Watershed"), family=fnt_header)


# ggplot Map --------------------------------------------------------------

# filter out negative numbers
shed_rivs2 <- filter(shed_rivs, streamorde>0)

ggplot() +
  geom_sf(data=h8_sel, fill=NA, color="gray50", linewidth=0.3, lty=2) +
  geom_sf(data=shed_rivs2, color="steelblue4", linewidth=shed_rivs2$streamorde/6, show.legend = FALSE)+
  geom_sf(data=ca_water_sel2, fill="cyan4", color="cyan4")+
  geom_sf(data=shed_wb, fill=alpha("steelblue2", 0.9), color="steelblue2")+
  geom_sf(data=h8_sel_mrg, fill=NA, color="gray40", linewidth=1.2)+
  theme_void(base_family = fnt_header) +
  labs(title = glue("{watershed} Watershed")) +
  theme(plot.title = element_text(face="bold", vjust=-0.5, hjust=0.3, size=14))
