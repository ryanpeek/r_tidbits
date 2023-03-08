# bob ross colors

# devtools::install_github("frankiethull/BobRossColors")

library(tidyverse)
library(sf)
library(geoarrow)
library(glue)
library(BobRossColors)

# Get Tues Data ------------------

tuesdata <- tidytuesdayR::tt_load('2023-02-21')
bob_ross <- tuesdata$bob_ross


# Load some Data ----------------------------------------------------------
hw <- geoarrow::read_geoparquet_sf("data_raw/healthy_watershed_2013.parquet")
h8 <- read_geoparquet_sf("data_raw/nhd_huc08.parquet") %>% st_transform(st_crs(hw))


# Select a Watershed ------------------------------------------------------
h8_select <- "Upper Cosumnes"

h8_trim <- h8 %>% filter(name %in% h8_select)

hw_trim <- hw[h8_trim,]


# View Bob Ross Paintings -------------------------------------------------

all_palettes |>
  select(painting_title) |>
  unique() |>
  pull()

# BobRossColors::all_palettes

br_cols <- BobRossColors::all_palettes %>%
  filter(painting_title %in% "autumn_splendor") %>%
  pull(divergent)

br_pal <- colorRampPalette(br_cols, 10)

# PLOT --------------------------------------------------------------------

ggplot() +
  geom_sf(data=hw_trim, aes(fill=CA_MMIs_wMod_csv_NormWatershedVulnerability)) +
  cowplot::theme_map(font_family = "Roboto Condensed") +
  scale_fill_gradientn(colours = br_cols) +
  labs(subtitle = glue("{h8_select} Watershed with Bob Ross palette"))
