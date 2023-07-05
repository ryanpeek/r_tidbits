# Plotting EDDI Drought Indices

# Libraries ---------------------------------------------------------------

library(eddi) # devtools::install_github("earthlab/eddi")
library(tidyverse)
library(hrbrthemes) # devtools::install_github("hrbrmstr/hrbrthemes")
library(sf) # works with vector data
library(rnaturalearth) # for install: https://github.com/ropensci/rnaturalearth
library(glue)
library(stars)

# Get States --------------------------------------------------------------

us_states <- c("Oregon", "Nevada", "California")

ne_states("United States of America", returnclass = "sf") %>%
  filter(
    name %in% c(us_states)) %>%
  st_transform(4326) -> wus

border <- st_union(wus) %>%
  st_transform(crs = 4326)
#st_transform(crs = albersusa::us_laea_proj)

# get boundary box
bb <- st_bbox(border)

# Get Data 1: ------------------------------------------------------------

# Comparison dates
stdate1 <- "2011-06-01"
stdate2 <- "2015-06-01"
time_int <- "1 month"
mon <- month.name[month(ymd(stdate1))]

# get monthly data as a raster
drdat1 <- get_eddi(date = stdate1, timescale = time_int)
drdat2 <- get_eddi(date = stdate2, timescale = time_int)

# convert to stars and trim
drtrim1 <- st_as_stars(drdat1)
drtrim1 <- drtrim1[border] # mask by border
plot(drtrim1, col=viridis::viridis(n = 5, option = "A"))

# convert
drtrim2 <- st_as_stars(drdat2)
drtrim2 <- drtrim2[border] # mask by border
#plot(drtrim2, col=terrain.colors(11))

# convert to sf polys: (merge polygons that have identical pixel values w merge=TRUE)
#eddi_sfdf1 <- st_as_sf(drtrim1, as_points = FALSE, merge = FALSE)
#eddi_sfdf2 <- st_as_sf(drtrim2, as_points = FALSE, merge = FALSE)

# convert to dataframe
eddi_df1 <- as.data.frame(drtrim1)
colnames(eddi_df1) <- c("x", "y", "value")
eddi_df2 <- as.data.frame(drtrim2)
colnames(eddi_df2) <- c("x", "y", "value")
#summary(eddi_df2)

# GGPLOT ------------------------------------------------------------------

(g1 <- ggplot() +
  geom_sf(data = wus, fill = "black", color = "#2b2b2b", size = 0.12) +
  geom_tile(data = eddi_df1, aes(x, y, fill = value)) +
  geom_sf(data = wus, fill = NA, color = "#b2b2b2", size = 0.125) +
  geom_sf(data = border, fill = NA, color = "white", size = .25) +
  scale_fill_viridis_c(name = "EDDI", option = "A", limits=c(-2.5,2.5), na.value = "#252a32") +
  coord_sf(crs = 4326, datum = NA) +
  guides(fill = guide_colourbar(title.position = "top")) +
  labs(
    x = NULL, y = NULL,
    title = glue("{stdate1}: EDDI (monthly)"),
    caption = "EDDI: Evaporative Demand Drought Index of\nhow anomalous the atmospheric evaporative\ndemand is (the thirst of the atmosphere)\nData: <https://www.earthdatascience.org/eddi>\nCreated by R. Peek using #rstats") +
   # if you run into a font issue, change this to Arial or Times New Rom
   theme_ft_rc(base_family = "Roboto Slab", plot_title_size = 10, subtitle_size = 7, grid="") +
  #theme(legend.position = c(0.75, 0.7)) +
  theme(legend.direction = "vertical") +
  theme(legend.key.width = unit(1.2, "lines"),
        legend.key.height = unit(1, "lines"),
        panel.background = element_rect(color = "#252a32", fill = "#252a32"),
        plot.background = element_rect(fill = "#252a32")))

(g2 <- ggplot() +
    geom_sf(data = wus, fill = "black", color = "#2b2b2b", size = 0.12) +
    geom_tile(data = eddi_df2, aes(x, y, fill = value)) +
    geom_sf(data = wus, fill = NA, color = "#b2b2b2", size = 0.125) +
    geom_sf(data = border, fill = NA, color = "white", size = .25) +
    scale_fill_viridis_c(name = "EDDI", option = "A", limits=c(-2.5,2.5), na.value = "#252a32") +
    coord_sf(crs = 4326, datum = NA) +
    guides(fill = guide_colourbar(title.position = "top")) +
    labs(
      x = NULL, y = NULL,
      title = glue("{stdate2}: EDDI (monthly)"),
      #subtitle = "EDDI: Evaporative Demand Drought Index of\nhow anomalous the atmospheric evaporative\ndemand is (the thirst of the atmosphere)",
      caption = "EDDI: Evaporative Demand Drought Index of\nhow anomalous the atmospheric evaporative\ndemand is (the thirst of the atmosphere)\nData: <https://www.earthdatascience.org/eddi>\nCreated by R. Peek using #rstats") +
    # if you run into a font issue, change this to Arial or Times New Roman
    theme_ft_rc(base_family = "Roboto Slab", plot_title_size = 10, subtitle_size = 7, grid="") +
    #theme(legend.position = c(0.75, 0.7)) +
    theme(legend.direction = "vertical") +
    theme(legend.key.width = unit(1.2, "lines"),
          legend.key.height = unit(1, "lines")) +
    theme(panel.background = element_rect(color = "#252a32", fill = "#252a32")))


# patchwork ---------------------

library(patchwork)

# plot
patch1 <- (g1 + theme(legend.position = "none",
            plot.caption = element_blank()) +
    g2 + theme(plot.subtitle = element_blank(),
             plot.caption = element_blank()))
patch1

(patchplot <- patch1 +
  plot_annotation(title = "EDDI: Evaporative Demand Drought Index",
                  caption =  "A measure of the atmospheric evaporative demand or 'the thirst of the atmosphere'. \nData: <https://www.earthdatascience.org/eddi>  |  By: @riverpeek@vis.social",
                  theme = theme(plot.title = element_text(family = 'Roboto Slab', size = 18, color = "white"),
                                plot.caption = element_text('Roboto Slab', size=12, color="white"),
                                plot.background = element_rect(color = "#252a32", fill = "#252a32"),
                                panel.background = element_rect(color = "#252a32", fill = "#252a32"))))

# save w ggsave
ggsave(patchplot, height = 8, width = 12, units="in", dpi=300, bg="#252a32",
                   filename = glue("figs/eddi_drought_westcoast_{year(ymd(stdate1))}_{year(ymd(stdate2))}_{mon}.png"))

# Cowplot -----------------------------------------------------------------

# library(cowplot)
#
# # make title
# cowtitle <- ggdraw() +
#   draw_label("Comparison of EDDI: Evaporative Demand Drought Index",
#              fontfamily = "Roboto Condensed", size=18, color="white", x=0.05, hjust=0) +
#   theme(plot.margin = margin(0, 0, 0, 1),
#         plot.background = element_rect(fill = "#252a32"))
#
# cowcaption <- ggdraw() +
#   draw_label("EDDI: Evaporative Demand Drought Index\n A measure of the atmospheric evaporative demand or 'the thirst of the atmosphere'. \nData: <https://www.earthdatascience.org/eddi>  |  By: @riverpeek@vis.social", fontfamily="Roboto Slab", size=13, y=0.2, x=0, hjust=-.1, vjust=-0.2, color = "white") +
#   theme(plot.margin = margin(0, 0, 0, 3),
#         plot.background = element_rect(fill = "#252a32"))
#
# # make plot
# (comb_plot <- plot_grid(
#   g1 + theme(legend.position = "none",
#              plot.caption = element_blank()),
#   g2 + theme(plot.subtitle = element_blank(),
#              plot.caption = element_blank()),
#   nrow = 1, align="h") +
#     theme(plot.background = element_rect(fill = "#252a32"),
#           plot.margin = margin(0, 0, 0, 3)))
#
# # combine
# #plot_grid(cowtitle, comb_plot, cowcaption, ncol=1, rel_heights = c(0.1, 0.8, 0.1))
# plot_grid(comb_plot, cowcaption, ncol=1, rel_heights = c(1, 0.1))
#
# # save
# cowplot::save_plot(
#   plot = comb_plot, base_height = 8, base_width = 12, units="in", dpi=300,
#   filename = glue("figs/eddi_drought_westcoast_{year(ymd(stdate1))}_{year(ymd(stdate2))}_{mon}_cowplot.pdf"),
#   device = cairo_pdf
# )
