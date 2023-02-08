# us states

library(tidyverse)
library(sf)
#library(albersusa)
#library(USAboundaries)
library(tigris)
options(tigris_use_cache = TRUE)
library(tidycensus)

us_states <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

# Shift but preserve area
us_states_eqarea <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry(preserve_area = TRUE)


# Shift and rescale but position AK/HI/PR outside the continental US rather than below
us_states_outside <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry(position = "outside")

# Plot All ----------------------------------------------------------------

ggplot() +
  geom_sf(data = us_states, fill=NA, color="black", size=1) +
  geom_sf(data = us_states_eqarea, fill=NA, color="orange", size=0.7) +
  geom_sf(data = us_states_outside, fill=NA, color="blue", size=0.2) +
  theme_void()


# Fancy Map ---------------------------------------------------------------

# Shift a dataset obtained outside tigris and make a map
income_by_metro <- get_acs(
  geography = "cbsa",
  variables = "B01002_001",
  geometry = TRUE
) %>%
  shift_geometry()

ggplot() +
  geom_sf(data = income_by_metro, aes(fill = estimate), color = NA) +
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
  scale_fill_viridis_c() +
  theme_void(base_size = 16) +
  labs(title = "Median age by CBSA, 2015-2019 ACS",
       fill = "ACS estimate  ",
       caption = "Note: Alaska, Hawaii, and Puerto Rico are shifted and not to scale.") +
  theme(plot.title = element_text(hjust = 0.5))



us_states_outside <- states(cb = TRUE, resolution = "20m") %>% 
  shift_geometry(preserve_area = TRUE,
                 position = "outside")

ggplot(us_states_outside) + 
  geom_sf() + 
  theme_void()
