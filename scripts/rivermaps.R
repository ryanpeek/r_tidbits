# river mapping
#https://milospopovic.net/map-rivers-with-sf-and-ggplot2-in-r/

suppressPackageStartupMessages({
  library("httr");
  library("tidyverse");
  library("sf");
  library("fs");
  library("glue")
})

#switch off S2
sf::sf_use_s2(FALSE)

# Get Rivers --------------------------------------------------------------

# GLORIC hydrosheds data: https://hydrosheds.org/page/gloric
# LINE RIVER data: https://hydrosheds.org/page/hydrorivers

# a path to file:
ptofile<-"data_raw/HydroRIVERS_v10_eu_geodatabase/HydroRIVERS_v10_eu.gdb/"

get_data <- function(file) {
  rivers <- st_read(ptofile) %>%
    st_cast("MULTILINESTRING")
  return(rivers)
}

eu_rivers <- get_data(ptofile)

# Crop to a Country -------------------

youpick <- "Poland"

cntry <- rnaturalearth::ne_countries(country = youpick, returnclass = "sf") %>%
  st_transform(4326)

# make a bbox
bbox <- st_bbox(cntry)
bbox
# plot(cntry$geometry)

# crop
eu_rivs <- eu_rivers[cntry,]

# plot(eu_rivs$Shape)

# Get HydroData River Widths -----------------------------------------------------------
eu_rivs <- eu_rivs %>%
  mutate(width = as.numeric(ORD_FLOW),
         width = case_when(width == 3 ~ 1,
                           width == 4 ~ 0.8,
                           width == 5 ~ 0.6,
                           width == 6 ~ 0.4,
                           width == 7 ~ 0.2,
                           width == 8 ~ 0.2,
                           width == 9 ~ 0.1,
                           width == 10 ~ 0.1,
                           TRUE ~ 0)) %>%
  st_as_sf()


# Make a Bounding Box -----------------------------------------------------

# get_bounding_box <- function(crsLONGLAT, bbox, new_prj, bb) {
#   crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
#   # this is europe
#   bbox <- st_sfc(
#     st_polygon(list(cbind(
#       c(-10.5, 48.5, 48.5, -10.5, -10.5),
#       c(35.000, 35.000, 69.5, 69.5, 35.000)
#     ))),
#     crs = crsLONGLAT)
#
#   #new_prj <- st_transform(bbox, crs = 4087)
#   #bb <- st_bbox(new_prj)
#   bb <- st_bbox(bbox)
#
#   return(bb)
# }
# bbox <- get_bounding_box()


# Plot --------------------------------------------------------------------


p <-
  ggplot() +
  geom_sf(data=eu_rivs, aes(color=as.factor(ORD_STRA))) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(y="", subtitle="",
       x = "",
       title=glue("Rivers of {youpick}"),
       caption="HydroSHEDS database http://www.hydrosheds.org") +
  scale_color_manual(
    name = "",
    values = rev(c('#08306b', '#08519c', '#2171b5', '#4292c6', '#6baed6', '#9ecae1', '#c6dbef'))) +
  #scale_size(range=c(0, .3)) +
  scale_alpha_manual(values=c("1" = 0.6, "2" = 0.7, "3" = .8, "4" = .9, "5" = 0.9, "6"=1, "7"=1)) +
  scale_size_manual(values=c("1" = 0.3, "2" = 0.4, "3" = .6, "4" = .8, "5" = 1, "6"=1.2, "7"=3)) +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Slab"),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size=20, color="#2171b5", hjust=0.5, vjust=0),
        plot.subtitle = element_text(size=14, color="#ac63a0", hjust=0.5, vjust=0),
        plot.caption = element_text(size=10, color="grey60", hjust=0.5, vjust=10),
        axis.title.x = element_text(size=10, color="grey20", hjust=0.5, vjust=-6),
        legend.text = element_text(size=9, color="grey20"),
        legend.title = element_text(size=10, color="grey20"),
        strip.text = element_text(size=12),
        plot.margin = unit(c(t=1, r=-2, b=-1, l=-2),"lines"),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

p

#ggsave(p, filename="european_rivers_new.png", width=7, height=8.5, dpi = 600)


# Tmap Version ------------------------------------------------------------

library(tmap)
library(tmaptools)

tm_shape(cntry) +
  tm_polygons(alpha=0, border.col = "gray20", lwd=7)+
tm_shape(eu_rivs) +
  tm_lines(lwd="ORD_STRA",
           col="ORD_STRA", palette="Blues",
           legend.show=FALSE) +
  tm_layout(fontfamily = "Roboto Slab",
    title = glue("Rivers of {youpick}"),
    legend.show=FALSE,
    frame=FALSE)
