# light

library(terra)
library(tidyverse)
library(hrbrthemes)
library(rnaturalearth)
library(tigris)

# download data
# from this article: https://www.nature.com/articles/s41597-020-0510-y
# data: https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/7
r <- rast("https://figshare.com/ndownloader/files/17626016")

# get a country
v <- rnaturalearth::ne_states(country = "Sweden") |>
  vect()

# get a county
cnty <- counties(state = c("CA"))
cnty_a <- cnty %>% filter(NAME=="Yolo")

# crop by country
r2 <- r %>%
  crop(v)

r2_c <- r %>%
  crop(cnty_a)

# extract XY data
r_xy <- terra::extract(r2_c, cnty_a, xy = TRUE)
r_xy <- rename(r_xy, "val"=2)

# plot
ggplot() + geom_tile(data=r_xy, aes(x=x, y=y, fill=val)) +
  scale_fill_viridis_c("Light Intensity", option="A") +
  theme_ft_rc(base_family = "Cinzel Decorative") +
  labs(title="Lights in Yolo County", y="", x="")
ggsave(filename = "figs/lights_in_yolo_county.png", dpi=300,
       width = 10, height = 8)

# aggegrate to coarser res
r_xy_10 <- extract(aggregate(r2, fact = 10), v, xy = TRUE)
r_xy_10 <- rename(r_xy_10, "val"= 2)

# plot
ggplot() + geom_tile(data=r_xy_10, aes(x=x, y=y, fill=val)) +
  scale_fill_viridis_c(option="A")
