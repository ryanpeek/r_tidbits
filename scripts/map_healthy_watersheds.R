# healthy watersheds
# full report here: https://www.mywaterquality.ca.gov/monitoring_council/healthy_streams/docs/ca_hw_report_111213.pdf
# zip is here: https://databasin.org/datasets/84270b4364174451ae978f9872e30574/

# here for EPA's version
# https://www.epa.gov/sites/production/files/2017-06/ca_phwa_package_170518.zip


# Libraries ---------------------------------------------------------------

library(sf)
library(glue)
library(showtext)
showtext_opts(dpi=300)
# font_paths()
#library(extrafont)
#font_import(pattern = "Atkinson")
# run once to import all fonts to R: font_import(prompt=FALSE)
library(tidyverse)
library(geoarrow)
library(mapview)
mapviewOptions(fgb=TRUE)

# Read and Save Data ------------------------------------------------------

# # path to db
# db <- r"(C:\Users\RPeek\OneDrive - California Department of Fish and Wildlife\Documents\DATA\Rel_Watershed_VulnIndex-CA_Integrated_Assessment_of Watershed Health\data\v101\hwi_california.gdb)"
# db <- "data_raw/hwi_california.gdb"
# # # layers
# st_layers(db)
# #
# # # read in
# hw_df <- st_read(db, "CA_MMIs_wMod")
# st_crs(hw_df)
# summary(hw_df$CA_MMIs_wMod_csv_Fire)
# # save
# write_geoparquet(hw_df, "data_raw/healthy_watershed_2013.parquet")


# Get Data ----------------------------------------------------------------

# read
hw_df <- read_geoparquet_sf("data_raw/healthy_watershed_2013.parquet")

# regions
table(hw_df$PSARegion)
table(hw_df$WQRegion)

# select a region
region_sel <- "West Sierra"

# plot a region
# plot(hw_df[hw_df$PSARegion==region_sel,]$Shape)

# mapview
#hw_df %>% filter(PSARegion %in% region_sel) %>%
#  mapview(., zcol="CA_MMIs_wMod_csv_NormWatershedVulnerability")

#hw_df %>% filter(PSARegion=="Central Valley") %>%
#  mapview(., zcol="CA_MMIs_wMod_csv_NormWatershedVulnerability")

# Add Circular Inset ------------------------------------------------------

# grab a catchment id
sel_id <- 15014869

# make a circle buffer
center_proj <- st_centroid(hw_df %>% filter(CA_MMIs_wMod_csv_CATCHID==sel_id))
dist <-  10000 # the buffer
circle_buff <- center_proj %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 3310)
plot(circle_buff$Shape)

# crop the data
hw_df_cropped <- hw_df %>%
  filter(PSARegion %in% c(region_sel)) %>%
  st_intersection(circle_buff)

mapview(hw_df_cropped, zcol="CA_MMIs_wMod_csv_NormWatershedVulnerability") # check


# Make a Nice Map ---------------------------------------------------------
# deal with fonts
#font_add(family = "Atkinson Hyperlegible", regular = "Atkinson-Hyperlegible-Regular-102.ttf")
#font_add(family = "Roboto Condensed", regular = "RobotoCondensed-Regular.ttf")
#font_files() %>% filter(grepl("Atkinson", ps_name))
#font_files() %>% filter(grepl("Roboto Condensed", family))
showtext::showtext_auto()

#extrafont::loadfonts(quiet=TRUE)
(main_map <- hw_df %>% filter(PSARegion %in% c(region_sel)) %>%
    ggplot() +
    geom_sf(
      # already normalized:
      aes(fill = CA_MMIs_wMod_csv_NormWatershedHealth),
      lwd = 0,
      color = alpha("white",0.1)) +
    labs(
      title = glue("Normalized Watershed Health: {region_sel}"),
      caption = "Healthy Watershed Data | Graphic by R. Peek")+
    scale_fill_gradientn(
      colors = rev(c("#9DBF9E", "#FCB97D", "#A84268")),
      # Redefine the fill colour for NA values
      na.value = "grey80",
      # Set the scale limits to be 0 to x%
      limits = c(0, 0.8),
      # Set the out-of-bounds ("oob") rule to squish out-of-bounds values to the nearest limit
      oob = scales::squish,
      # Format labels as percentages
      labels = scales::percent,
      # Give the scale a title
      name = "HW: Watershed Health") +
    # add the outline
    geom_sf(data=circle_buff, fill=NA, col="black", linewidth=.5)+
    coord_sf(expand = FALSE) +
    theme_void() +
    theme(
      # defines the edge of the legend.position coordinates
      #legend.justification = c(0.2, 1),
      legend.position = c(0.2, .25),
      title = element_text(family = "Atkinson Hyperlegible"),
      legend.title = element_text(family = "Roboto", size = 10, face = "bold"),
      legend.text = element_text(family = "Roboto Condensed", size = 10)
    ))


# Make Circular Plot ------------------------------------------------------
#extrafont::loadfonts(quiet=TRUE)

(circ_plot <- hw_df_cropped %>%
   ggplot() +
   geom_sf(
     # already normalized:
     aes(fill = CA_MMIs_wMod_csv_NormWatershedHealth),
     lwd = 0,
     color = alpha("white",0.1), show.legend = FALSE) +
   scale_fill_gradientn(
     colors = rev(c("#9DBF9E", "#FCB97D", "#A84268")),
     # Redefine the fill colour for NA values
     na.value = "grey80",
     # Set the scale limits to be 0 to x%
     limits = c(0, 0.8),
     # Set the out-of-bounds ("oob") rule to squish out-of-bounds values to the nearest limit
     oob = scales::squish) +
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
ggsave(plot = p1, filename = "figs/map_hw_health_zoom_west_sierra.pdf",
       device = cairo_pdf, bg="white",
       width = 8.5, height = 10)

# osx
ggsave(plot = p1, filename = "figs/map_hw_health_zoom_west_sierra1.png",
       bg="white", dpi=300, device = "png",
       width = 8.5, height = 10, units = "in")

# windows works, but drops title in osx
ggsave(plot = p1, filename = "figs/map_hw_health_zoom_west_sierra.png",
      type="cairo", dpi=300,
      width = 8.5, height = 10, units = "in")
