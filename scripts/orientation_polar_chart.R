# CODE ADAPTED FROM
# https://github.com/USGS-VIZLAB/chart-challenge-22/tree/main/11_circular_lkoenig
# @lkoenig
# BY R. PEEK, 2022-04-12

library(tidyverse)
library(patchwork)
library(nhdplusTools)
library(sf)
library(ggspatial)
library(showtext)

# Source helper functions
source("~/Downloads/orientation_helpers.R")

# Set up fonts for plots
font_add_google('Source Sans Pro', regular.wt = 300, bold.wt = 700)
showtext_auto()

# Define huc8 sub-basins
# note: HUC8 subsets from the watershed boundary dataset (WBD) were used for
# convenience for this river orientation visualization, to represent a reasonable,
# "medium" scale of analysis and so that a user wouldn't need to have the national
# hydrography dataset (NHDPlusv2) downloaded locally. The selected HUC8 id's
# represent 'whole' watersheds/river networks, but not all HUC8 subsets do (i.e.,
# they may represent just a portion of a watershed).
huc8_tbl <- tibble(huc8_id = c("18010106","17100311",
                               "18010101","18010102","18020003",
                               "18010211","18020121",
                               "18020122",
                               "18020123","18020125","18020126",
                               "18020128", "18020129"),
                   huc8_name = c("South Fork Eel","Illinois",
                                 "Smith","Mad-Redwood", "Lower Pit",
                                 "Trinity", "North Fork Feather",
                                 "East Branch North Fork Feather",
                                 "Middle Fork Feather","Upper Yuba", "Upper Bear",
                                 "North Fork American","South Fork American"))

# Fetch NHDv2 flowlines for each huc8 basin
# another function (fetch_flowlines_nwis) can also be used to fetch flowlines
# upstream of a selected NWIS gage.
flines <- lapply(huc8_tbl$huc8_id, fetch_flowlines)

# Estimate channel orientation (i.e., azimuth) for each huc8 basin
# note that this step is taking a while to run, ~10 min?
flines_azimuth <- lapply(flines, function(x){
  az_df <- x %>%
    split(., 1:length(.$geometry)) %>%
    purrr::map_dfr(~mutate(., azimuth = calc_azimuth_circ_mean(.)))
  return(az_df)
})

# Format channel orientation table
flines_azimuth_df <- do.call("rbind", flines_azimuth) %>%
  select(huc8_id, azimuth, streamorde, lengthkm) %>%
  # add huc8 name to this table
  left_join(huc8_tbl, by = "huc8_id") %>%
  # define order of huc8's
  mutate(huc8_name_ord = factor(huc8_name,
                           levels = c("Illinois","Smith","Mad-Redwood","Lower Pit",
                                      "Trinity", "South Fork Eel",
                                      "North Fork Feather",
                                      "East Branch North Fork Feather",
                                      "Middle Fork Feather","Upper Yuba", "Upper Bear",
                                      "North Fork American","South Fork American"))) %>%
  relocate(geometry, .after = last_col())

# We were getting inconsistent errors with this data pull
# Data can also be downloaded from s3 with the following lines
# download.file('https://labs.waterdata.usgs.gov/visualizations/data/flines_azimuth_df.rds', 'data/flines_azimuth_df.rds')
#flines_azimuth_df <- readRDS('data/flines_azimuth_df.rds')

# Assemble plot

# A couple steps so that coord_polar will allow free scales for facets,
# grabbed from https://github.com/tidyverse/ggplot2/issues/2815
cp <- coord_polar()
cp$is_free <- function() TRUE

# Create grid containing channel orientation plots
azimuth_grid <- plot_azimuth(flines_azimuth_df %>%
                               filter(huc8_name != 'Mad-Redwood'),
                             cp, fill = "#105073", color = "#09344E") +
  facet_wrap(~huc8_name_ord, scales = "free_y", ncol = 4) +
  theme(text = element_text(size = 20),
        strip.text.x = element_text(size = 42, face = "bold"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        aspect.ratio = 1,
        panel.grid = element_line(size = 0.2),
        axis.text.x = element_text(size = 20))
azimuth_grid

# Save plots
ggsave(glue::glue("figs/azimuth_grid.png"),
       plot = azimuth_grid,bg = "white",
       width = 12, height = 12, units = c("in"),
       dpi = 300)


# NF American -------------------------------------------------------------


# Create "legend" inset plot that explains how to read the polar histograms
inset_ntw_plot <- plot_ntw(filter(flines_azimuth_df, huc8_name == "North Fork American"))
inset_polar_plot <- plot_azimuth(filter(flines_azimuth_df,huc8_name == "North Fork American"),
                                 fill = "#105073", color = "#09344E") +
  theme(plot.margin = unit(c(t=-4, r=15, b=-4, l=0), "lines"),
        text = element_text(colour = "white", size = 40),
        plot.background = element_blank(),
        axis.text = element_text(color="white"),
        panel.background = element_blank(),
        aspect.ratio = 1)

inset_plot1 <- inset_polar_plot + inset_element(inset_ntw_plot, 0.5, 0.4, 1, 1, align_to = 'full') +
  plot_annotation(title = expression("The NF American River (CA) generally flows in the"~bold("southwest")~"direction"),
                  subtitle = "from its headwaters just east of Lake Tahoe.",
                  caption = 'The direction of each bar in the polar histogram represents the river orientation and the\nlength of each bar represents the proportion of total river length with that orientation.',
                  theme = theme(plot.title = element_text(colour = "white",size = 24,
                                                          hjust = 0, margin = margin(0,0,0.1,0)),
                                plot.subtitle = element_text(colour = "white",
                                                             size = 24, hjust = 0),
                                plot.caption = element_text(colour = "white",
                                                            size = 20, hjust = 0, lineheight = 0.5),
                                plot.background = element_blank(),
                                panel.background = element_blank(),
                                aspect.ratio = 1))
inset_plot <- inset_plot1 + plot_compass(text_size = 11)
inset_plot

# NF Feather -------------------------------------------------------------

# Create "legend" inset plot that explains how to read the polar histograms
inset_ntw_plot <- plot_ntw(filter(flines_azimuth_df, huc8_name == "North Fork Feather"))
inset_polar_plot <- plot_azimuth(filter(flines_azimuth_df,huc8_name == "North Fork Feather"),
                                 fill = "#105073", color = "#09344E") +
  theme(plot.margin = unit(c(t=-4, r=15, b=-4, l=0), "lines"),
        text = element_text(colour = "white", size = 40),
        plot.background = element_blank(),
        axis.text = element_text(color="white"),
        panel.background = element_blank(),
        aspect.ratio = 1)

inset_plot1 <- inset_polar_plot + inset_element(inset_ntw_plot, 0.5, 0.4, 1, 1, align_to = 'full') +
  plot_annotation(title = expression("The NF Feather River (CA) generally flows in the"~bold("southern")~"direction"),
                  subtitle = "from its headwaters.",
                  caption = 'The direction of each bar in the polar histogram represents the river orientation and the\nlength of each bar represents the proportion of total river length with that orientation.',
                  theme = theme(plot.title = element_text(colour = "white",size = 24,
                                                          hjust = 0, margin = margin(0,0,0.1,0)),
                                plot.subtitle = element_text(colour = "white",
                                                             size = 24, hjust = 0),
                                plot.caption = element_text(colour = "white",
                                                            size = 20, hjust = 0, lineheight = 0.5),
                                plot.background = element_blank(),
                                panel.background = element_blank(),
                                aspect.ratio = 1))
inset_plot <- inset_plot1 + plot_compass(text_size = 11)
inset_plot

# MF Feather -------------------------------------------------------------

# Create "legend" inset plot that explains how to read the polar histograms
inset_ntw_plot <- plot_ntw(filter(flines_azimuth_df, huc8_name == "Middle Fork Feather"))
inset_polar_plot <- plot_azimuth(filter(flines_azimuth_df,huc8_name == "Middle Fork Feather"),
                                 fill = "#105073", color = "#09344E") +
  theme(plot.margin = unit(c(t=-4, r=15, b=-4, l=0), "lines"),
        text = element_text(colour = "white", size = 40),
        plot.background = element_blank(),
        axis.text = element_text(color="white"),
        panel.background = element_blank(),
        aspect.ratio = 1)

inset_plot1 <- inset_polar_plot + inset_element(inset_ntw_plot, 0.5, 0.4, 1, 1, align_to = 'full') +
  plot_annotation(title = expression("The MF Feather River (CA) generally flows in the"~bold("west and southwest")~"direction"),
                  subtitle = "from its headwaters.",
                  caption = 'The direction of each bar in the polar histogram represents the river orientation and the\nlength of each bar represents the proportion of total river length with that orientation.',
                  theme = theme(plot.title = element_text(colour = "white",size = 24,
                                                          hjust = 0, margin = margin(0,0,0.1,0)),
                                plot.subtitle = element_text(colour = "white",
                                                             size = 24, hjust = 0),
                                plot.caption = element_text(colour = "white",
                                                            size = 20, hjust = 0, lineheight = 0.5),
                                plot.background = element_blank(),
                                panel.background = element_blank(),
                                aspect.ratio = 1))
inset_plot <- inset_plot1 + plot_compass(text_size = 11)
inset_plot


# Save Out ----------------------------------------------------------------

river <- "mffeather"

ggsave(glue::glue("figs/azimuth_inset_map_{river}.png"),
       plot = inset_ntw_plot, bg="black",
       width = 6, height = 4, units = c("in"),
       dpi = 300)

ggsave(glue::glue("figs/azimuth_inset_plot_{river}.png"),
       plot = inset_plot, bg = "black",
       width = 6, height = 4, units = c("in"),
       dpi = 300)





