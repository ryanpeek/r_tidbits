# river mapping
#https://milospopovic.net/map-rivers-with-sf-and-ggplot2-in-r/

libs <- c("httr", "tidyverse", "sf", "fs")
invisible(lapply(libs, library, character.only = T))

#switch off S2
sf::sf_use_s2(FALSE)

# Get Rivers --------------------------------------------------------------

# GLORIC hydrosheds data: https://hydrosheds.org/page/gloric
# LINE RIVER data: https://hydrosheds.org/page/hydrorivers

# a path to file:
ptofile<-"data/HydroRIVERS_v10_eu_geodatabase/HydroRIVERS_v10_eu.gdb/"

get_data <- function(file) {
  rivers <- st_read(ptofile) %>%
    st_cast("MULTILINESTRING")
  return(rivers)
}

eu_rivers <- get_data(ptofile)


# Get HydroData River Widths -----------------------------------------------------------
eu_riv <- eu_rivers %>%
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

get_bounding_box <- function(crsLONGLAT, bbox, new_prj, bb) {

  crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

  # this is europe
  bbox <- st_sfc(
    st_polygon(list(cbind(
      c(-10.5, 48.5, 48.5, -10.5, -10.5),
      c(35.000, 35.000, 69.5, 69.5, 35.000)
    ))),
    crs = crsLONGLAT)

  #new_prj <- st_transform(bbox, crs = 4087)
  #bb <- st_bbox(new_prj)
  bb <- st_bbox(bbox)

  return(bb)
}
bbox <- get_bounding_box()
bbox

# Plot --------------------------------------------------------------------


p <-
  ggplot() +
  geom_sf(data=eu_riv, aes(color=factor(ORD_FLOW), size=width)) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(y="", subtitle="",
       x = "",
       title="Rivers of Europe",
       caption="HydroSHEDS database http://www.hydrosheds.org") +
  scale_color_manual(
    name = "",
    values = c('#08306b', '#08519c', '#2171b5', '#4292c6', '#6baed6', '#9ecae1', '#c6dbef', '#deebf7')) +
  scale_size(range=c(0, .3)) +
  scale_alpha_manual(values=c("3" = 1, "4" = 1, "5" = .7, "6" = .6, "7" = .4, "8" = .3, "9" = .2, "10" = .1)) +
  theme_minimal() +
  theme(text = element_text(family = "georg"),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size=40, color="#2171b5", hjust=0.5, vjust=0),
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

ggsave(p, filename="european_rivers_new.png", width=7, height=8.5, dpi = 600)
