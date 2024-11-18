# Plotting EDDI Drought Indices

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(hrbrthemes) # devtools::install_github("hrbrmstr/hrbrthemes")
library(sf) # works with vector data
library(rnaturalearth) # for install: https://github.com/ropensci/rnaturalearth
library(glue)
library(stars)
library(terra)


# EDDI Function -----------------------------------------------------------

# https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/
# example file name: EDDI_ETrs_02wk_19801215.asc

# write a function:
get_eddi <- function(stdate, time_int, time_dur){
  stdate <- stdate
  yr <- lubridate::year(lubridate::ymd(stdate))
  time_int <- time_int # week: wk, month: mn
  time_dur <- time_dur # two digit number
  time_dur <- stringr::str_pad(time_dur, width = 2, side = "left", pad = 0)
  eddi_path <- "https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data"
  dat_path <- glue::glue("{eddi_path}/{yr}/EDDI_ETrs_{time_dur}{time_int}_{stdate}.asc")

  # download to temp
  temp_file_path <- tempfile(fileext = ".asc")
  download.file(dat_path, destfile = temp_file_path)

  eddi_dat <- terra::rast(temp_file_path)
  return(eddi_dat)
}


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

# Get EDDI & PLOT -------------------------------------------------------

# set the time interval
time_int <- "mn" # week: wk, month: mn
time_dur <- 12 # two digit number

## TIME 1 --------------------------

# params
stdate1 <- "20140715"

# try function:
eddi_dat1 <- get_eddi(stdate1, time_int = time_int, time_dur = time_dur)

# plot(eddi_dat) # all CONUS
eddi_crop1 <- terra::crop(eddi_dat1, vect(border))
# plot(eddi_crop) # WEST COAST
eddi_mask1 <- terra::mask(eddi_crop1, vect(border))
# plot(eddi_mask) # masked to just states of interest

## Extract to dataframe from raster

eddi_df1 <- extract(eddi_dat1, vect(border), method="bilinear", xy=TRUE, ID=FALSE) |>
  mutate(date = ymd(stdate1)) |> rename(eddi=1)

(g1 <- ggplot() +
  geom_sf(data = wus, fill = "black", color = "#2b2b2b", size = 0.12) +
  geom_tile(data = eddi_df1, aes(x, y, fill = eddi)) +
  geom_sf(data = wus, fill = NA, color = "#b2b2b2", size = 0.125) +
  geom_sf(data = border, fill = NA, color = "white", size = .25) +
  scale_fill_viridis_c(name = "EDDI", option = "A", limits=c(-2.5,2.5), na.value = "#252a32") +
  coord_sf(crs = 4326, datum = NA) +
  guides(fill = guide_colourbar(title.position = "top")) +
  labs(
    x = NULL, y = NULL,
    title = glue("{ymd(stdate1)}: EDDI ({time_dur}-{time_int})"),
    caption = "EDDI: Evaporative Demand Drought Index of\nhow anomalous the atmospheric evaporative\ndemand is (the thirst of the atmosphere)\nData: <https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/") +
   # if you run into a font issue, change this to Arial or Times New Rom
   theme_ft_rc(base_family = "Rockwell", plot_title_size = 10, subtitle_size = 7, grid="") +
  #theme(legend.position = c(0.75, 0.7)) +
  theme(legend.direction = "vertical") +
  theme(legend.key.width = unit(1.2, "lines"),
        legend.key.height = unit(1, "lines"),
        panel.background = element_rect(color = "#252a32", fill = "#252a32"),
        plot.background = element_rect(fill = "#252a32")))


## TIME 2 --------------------------

# date param
stdate2 <- "20240715"

# try function:
eddi_dat2 <- get_eddi(stdate2, time_int = time_int, time_dur = time_dur)

# plot(eddi_dat) # all CONUS
eddi_crop2 <- terra::crop(eddi_dat2, vect(border))
# plot(eddi_crop) # WEST COAST
eddi_mask2 <- terra::mask(eddi_crop2, vect(border))
# plot(eddi_mask) # masked to just states of interest

## Extract to dataframe from raster
eddi_df2 <- extract(eddi_dat2, vect(border), method="bilinear", xy=TRUE, ID=FALSE) |>
  mutate(date = ymd(stdate2)) |> rename(eddi=1)

## Make a ggplot2
(g2 <- ggplot() +
    geom_sf(data = wus, fill = "black", color = "#2b2b2b", size = 0.12) +
    geom_tile(data = eddi_df2, aes(x, y, fill = eddi)) +
    geom_sf(data = wus, fill = NA, color = "#b2b2b2", size = 0.125) +
    geom_sf(data = border, fill = NA, color = "white", size = .25) +
    scale_fill_viridis_c(name = "EDDI", option = "A", limits=c(-2.5,2.5), na.value = "#252a32") +
    coord_sf(crs = 4326, datum = NA) +
    guides(fill = guide_colourbar(title.position = "top")) +
    labs(
      x = NULL, y = NULL,
      title = glue("{ymd(stdate2)}: EDDI ({time_dur}-{time_int})"),
      caption = "EDDI: Evaporative Demand Drought Index of\nhow anomalous the atmospheric evaporative\ndemand is (the thirst of the atmosphere)\nData: <https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/") +
    # if you run into a font issue, change this to Arial or Times New Rom
    theme_ft_rc(base_family = "Rockwell", plot_title_size = 10, subtitle_size = 7, grid="") +
    #theme(legend.position = c(0.75, 0.7)) +
    theme(legend.direction = "vertical") +
    theme(legend.key.width = unit(1.2, "lines"),
          legend.key.height = unit(1, "lines"),
          panel.background = element_rect(color = "#252a32", fill = "#252a32"),
          plot.background = element_rect(fill = "#252a32")))


# Patchwork ---------------------

library(patchwork)

# simple plot
patch1 <- (g1 + theme(legend.position = "none",
            plot.caption = element_blank()) +
    g2 + theme(plot.subtitle = element_blank(),
             plot.caption = element_blank()))
# patch1

# now add detals
(final_plot <- patch1 +
  plot_annotation(title = "EDDI: Evaporative Demand Drought Index",
                  caption =  "A measure of the atmospheric evaporative demand or 'the thirst of the atmosphere'. \nData: <https://downloads.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/>  |  R. Peek, 2024",
                  theme = theme(plot.title = element_text(family = 'Rockwell', size = 18, color = "white"),
                                plot.caption = element_text('Rockwell', size=12, color="white"),
                                plot.background = element_rect(color = "#252a32", fill = "#252a32"),
                                panel.background = element_rect(color = "#252a32", fill = "#252a32"))))


# Save --------------------------------------------------------------------

ggsave(final_plot, height = 8, width = 11, units="in", dpi=300, bg="#252a32",
       filename = glue("figs/eddi_drought_westcoast_{year(ymd(stdate1))}_{year(ymd(stdate2))}_{time_dur}{time_int}.png"))


