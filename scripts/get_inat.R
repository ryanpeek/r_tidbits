# inat

library(rinat)
library(tigris)
library(mapview)
mapviewOptions(fgb=FALSE)
library(tidyverse)
library(sf)

# Get a County ------------------------------------------------------------

ca_eldor <- tigris::counties("CA") %>%
  filter(NAME=="El Dorado")

bounds <- ca_eldor


# Get Data ----------------------------------------------------------------

# search by area
lica <- get_inat_obs(query="Bullfrog", bounds = bounds)

# plot
ggplot() + geom_point(data=lica, aes(x=longitude, y=latitude), alpha=0.5, col="green4") +
  geom_sf(data=ca_eldor, fill=NA)

