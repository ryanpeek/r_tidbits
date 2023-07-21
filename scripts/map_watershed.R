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
ca_cnty <- read_geoparquet_sf("data_raw/ca_cntys.parquet")

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
# mapview::mapview(h8) # can view with mapview 18060006: Central Coastal 18060005: Salinas
watershed <- c("Salinas", "Central Coastal")
h8_sel <- h8 %>% filter(name %in% watershed)
plot(h8_sel$geometry)
h8_sel_mrg <- rmapshaper::ms_dissolve(h8_sel)
plot(h8_sel_mrg$geometry)

# get water data
# ca_water <- tigris::area_water("CA", tigris::list_counties("CA")$county)
# # save out water data
# write_geoparquet(ca_water, here::here("data_raw/tigris_ca_cnty_water_data.parquet"))
ca_water <- read_geoparquet_sf(here("data_raw/tigris_ca_cnty_water_data.parquet"))
ca_water <- st_transform(ca_water, 3310)

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
f_get_fonts()

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
  theme_void(base_family = fnt_text) +
  labs(title = glue("{watershed} Watershed")) +
  theme(plot.title = element_text(face="bold", vjust=-0.5, hjust=0.3, size=14))



# Add Circular Inset ------------------------------------------------------

# use a lat lon
pt <- sf::st_as_sf(data.frame(x = -121.27190, y = 35.85476),
             coords = c("x", "y"), crs = 4326)

# make a circle buffer
center_proj <- pt
dist <-  10000 # the buffer
circle_buff <- center_proj %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 3310)
plot(circle_buff$geometry)

# crop the data
shed_rivs_crop <- shed_rivs2 %>%
  st_intersection(circle_buff)
ca_water_crop <- ca_water_sel2 %>%
  st_intersection(circle_buff)


# Main Map before Circle ------------------------
(main_map <-
   ggplot() +
   geom_sf(data=h8_sel, lwd = 0, color = alpha("white",0.1)) +
   labs(
     title = glue("{watershed} Watershed"),
     caption = "NHD Zoom | Graphic by R. Peek") +
   geom_sf(data=h8_sel, fill=NA, color="brown4", linewidth=0.2, lty=1) +
   geom_sf(data=shed_rivs2, color="steelblue4", linewidth=shed_rivs2$streamorde/6, show.legend = FALSE, alpha=0.6) +
   #geom_sf(data=ca_water_sel2, fill="cyan4", color="cyan4")+
   geom_sf(data=shed_wb, fill=alpha("steelblue2", 0.9), color="steelblue2")+
   # add the outline
   geom_sf(data=circle_buff, fill=NA, col="black", linewidth=.5)+
   coord_sf(expand = FALSE) +
   theme_void() +
   theme(
     # defines the edge of the legend.position coordinates
     #legend.justification = c(0.2, 1),
     legend.position = c(0.2, .25),
     title = element_text(family = "Roboto"),
     legend.title = element_text(family = "Roboto", size = 10, face = "bold"),
     legend.text = element_text(family = "Roboto Condensed", size = 10)
   ))


# Make Circular Plot ------------------------------------------------------

(circ_plot <-
   ggplot() +
   geom_sf(data=circle_buff, fill="white", col="black", linewidth=1)+
   geom_sf(data=shed_rivs_crop, color="steelblue4", linewidth=shed_rivs_crop$streamorde/6, show.legend = FALSE) +
   geom_sf(data=ca_water_crop, fill="cyan4", color="cyan4")+
   geom_sf(data=circle_buff, fill=NA, col="black", linewidth=1.5)+
   coord_sf(expand = 0.01) +
   theme_void()
)


# Combine w patchwork -----------------------------------------------------------------

library(patchwork)

# draw

(p1 <- main_map + inset_element(circ_plot,
                                left =  0.55, bottom = 0.6, 1.2, 1,
                                align_to = 'plot',
                                on_top = TRUE))

# save pdf
ggsave(plot = p1, filename = "figs/map_hw_health_zoom_west_sierra_w.pdf",
       device = cairo_pdf, bg="white",
       width = 8.5, height = 10)

# png
ggsave(plot = p1, filename = "figs/map_hw_health_zoom_west_sierra.png",
       bg="white", dpi=300, device = "png",
       width = 8.5, height = 10, units = "in")


