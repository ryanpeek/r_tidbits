# 03 Extract Drought Indices

#library(devtools)
#install_github('sbegueria/SPEI')
#install.packages('scPDSI')


# Libraries ---------------------------------------------------------------

library(eddi) # devtools::install_github("earthlab/eddi")
library(tidyverse)
library(hrbrthemes) # devtools::install_github("hrbrmstr/hrbrthemes")
library(sf)
library(rnaturalearth) # for install: https://github.com/ropensci/rnaturalearth
library(raster)
library(stars)
library(glue)


# Get States --------------------------------------------------------------

ne_states("United States of America", returnclass = "sf") %>%
  filter(
    name %in% c(
      "Oregon","California"
    )) %>%
  st_transform(4326) -> wus

border <- st_union(wus) %>%
  st_transform(crs = 4326)
#st_transform(crs = albersusa::us_laea_proj)

# Get Data: STARS ------------------------------------------------------------

# date
stdate <- "2022-12-01"
time_int <- "6 month"

# get monthly data
drdat <- get_eddi(date = stdate, timescale = time_int)
plot(drdat)

# crop to CA/OR border
drtrim <- crop(drdat, as(border, "Spatial")) # this trims to bbox of interest
plot(drtrim)

# save it out
#raster::writeRaster(drtrim, filename = glue("data/eddi_{stdate}_monthly_raster.tif"), overwrite=TRUE)

# convert to stars
drtrim <- st_as_stars(drtrim)
plot(drtrim)

# mask by border
bb <- st_bbox(border)
drtrim <- drtrim[border]

# convert to sf polys: (merge polygons that have identical pixel values w merge=TRUE)
eddi_sfdf <- st_as_sf(drtrim, as_points = FALSE, merge = FALSE)
plot(eddi_sfdf)

# save out
#saveRDS(eddi_sfdf, file = here::here(glue("data/eddi_{stdate}_monthly_sfpoly.rds")))

# convert to sf x.sf = st_xy2sfc(x, as_points = TRUE)
eddi_sfxy <- st_xy2sfc(drtrim, as_points = TRUE)
plot(eddi_sfxy)
#saveRDS(eddi_sfxy, file = here::here(glue("data/eddi_{stdate}_monthly_sfxy.rds")))

# convert to dataframe
eddi_spdf <- as.data.frame(drtrim)
colnames(eddi_spdf) <- c("x", "y", "value")
summary(eddi_spdf)
ggplot() + geom_tile(data=eddi_spdf, aes(x=x, y=y, fill=value)) +
  viridis::scale_fill_viridis(option = "A")

# save out
# saveRDS(eddi_spdf, file = here::here(glue("data/eddi_{stdate}_monthly_spdf.rds")))

# GGPLOT ------------------------------------------------------------------

(g1 <- ggplot() +
  geom_sf(data = wus, fill = "black", color = "#2b2b2b", size = 0.12) +
  geom_tile(data = eddi_spdf, aes(x, y, fill = value)) +
  geom_sf(data = wus, fill = NA, color = "#b2b2b2", size = 0.125) +
  geom_sf(data = border, fill = NA, color = "white", size = .25) +
  scale_fill_viridis_c(name = "EDDI", option = "A") +
  coord_sf(crs = 4326, datum = NA) +
  guides(fill = guide_colourbar(title.position = "top")) +
  labs(
    x = NULL, y = NULL,
    title = glue("{stdate}: EDDI (monthly)"),
    subtitle = "EDDI: Evaporative Demand Drought Index of\nhow anomalous the atmospheric evaporative\ndemand is (the thirst of the atmosphere)",
    caption = "Data: <https://www.earthdatascience.org/eddi>") +
  #expand_limits(x = 0, y=0)+
  theme_ft_rc(plot_title_size = 10, subtitle_size = 7, grid="") +
  #theme(legend.position = c(0.75, 0.7)) +
  theme(legend.direction = "vertical") +
  theme(legend.key.width = unit(1.2, "lines"),
        legend.key.height = unit(1, "lines")) +
  theme(panel.background = element_rect(color = "#252a32", fill = "#252a32")))

ed22 <- g1
ed21 <- g1

# Process and Plot --------------------------------------------------------

# if working with raster
# eddi <- mask(d19oct, as(border, "Spatial"))
# eddi <- projectRaster(eddi, crs = crs(albersusa::us_laea_proj))
# eddi <- mask(eddi, as(st_transform(border, crs(albersusa::us_laea_proj)), "Spatial"))
# eddi_spdf <- as.data.frame(as(eddi, "SpatialPixelsDataFrame"))
# colnames(eddi_spdf) <- c("value", "x", "y")
# saveRDS(eddi_spdf, here::here("data/eddi_20191115_monthly_spdf.rds"))
# d19oct <- readRDS(here::here("data/eddi_20191115_monthly_spdf.rds"))

# Cowplot -----------------------------------------------------------------

# month
mon <- "jun"

library(cowplot)

(comb_plot <- plot_grid(ed21, ed22, nrow = 1))

cowplot::save_plot(
  plot = comb_plot, base_height = 8, base_width = 12, units="in", dpi=300,
  filename = glue("figs/eddi_drought_westcoast_2021_2022_{mon}.pdf"),
  device = cairo_pdf
)



# Fasterize ---------------------------------------------------------------

library(fasterize)

d21oct <- read_rds("data/eddi_2021-06-01_monthly_sfpoly.rds") %>%
  rename(value=1)

ggplot() + geom_sf(data=d21oct, aes(fill=value), color="transparent", alpha=0.8) +
  viridis::scale_fill_viridis()

# need to get extent of raster:
r <- raster(d21oct, res = 1)
# rasterize
df21oct_r <- fasterize::fasterize(d21oct, r, field = "value")
# plot
plot(df21oct_r)
