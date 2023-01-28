# light

library(terra)
library(tidyverse)
library(hrbrthemes)
library(rnaturalearth)
library(tigris)
library(glue)


# Get Data ----------------------------------------------------------------

# download data
# from this article: https://www.nature.com/articles/s41597-020-0510-y
# data: https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/7
r <- rast("https://figshare.com/ndownloader/files/17626016")

cntry <- "Sweden"

# get a country
v <- rnaturalearth::ne_states(country = glue("{cntry}")) |>
  vect()

# get a county
state_sel <- "CA"
cnty_sel <- c("Placer", "El Dorado")
cnty <- counties(state = c(glue("{state_sel}")))
cnty_a <- cnty %>% filter(NAME %in% c(glue("{cnty_sel}")))


# Crop: by Country --------------------------------------------------------
# crop by country
r2 <- r %>%
  crop(v)

# CROP: By county ----------------------

# crop by county
r2_c <- r %>%
  crop(cnty_a)

## Extract by county -------------------------------------------------------

# extract XY data
r_xy <- terra::extract(r2_c, cnty_a, xy = TRUE)
r_xy <- rename(r_xy, "val"=2)


# Plot --------------------------------------------------------------------

# plot
ggplot() + geom_tile(data=r_xy, aes(x=x, y=y, fill=val)) +
  scale_fill_viridis_c("Light Intensity", option="A") +
  theme_ft_rc(base_family = "Cinzel Decorative") +
  labs(title=glue("Lights in {glue_collapse({cnty_sel}, ', ', last=' and ')} County, {state_sel}"), y="", x="")

ggsave(filename = glue("figs/lights_in_{cnty_sel}_county.png"), dpi=300,
       width = 10, height = 8)

# aggegrate to coarser res
r_xy_10 <- extract(aggregate(r2, fact = 10), v, xy = TRUE)
r_xy_10 <- rename(r_xy_10, "val"= 2)

# plot
ggplot() + geom_tile(data=r_xy_10, aes(x=x, y=y, fill=val)) +
  scale_fill_viridis_c(option="A")

# Extract by country -------------------------------------------------------

# extract XY data
r_xy <- terra::extract(r2, v, xy = TRUE)
r_xy <- rename(r_xy, "val"=2)

# plot
ggplot() + geom_tile(data=r_xy, aes(x=x, y=y, fill=val)) +
  scale_fill_viridis_c("Light Intensity", option="A") +
  theme_ft_rc(base_family = "Cinzel Decorative") +
  labs(title=glue("Lights in {cntry}"), y="", x="")
ggsave(filename = glue("figs/lights_in_{cntry}.png"), dpi=300,
       width = 10, height = 8)

# aggegrate to coarser res
r_xy_10 <- extract(aggregate(r2, fact = 10), v, xy = TRUE)
r_xy_10 <- rename(r_xy_10, "val"= 2)

# plot
ggplot() + geom_tile(data=r_xy_10, aes(x=x, y=y, fill=val)) +
  scale_fill_viridis_c(option="A")
