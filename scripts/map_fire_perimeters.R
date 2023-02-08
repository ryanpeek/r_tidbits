# fire perimeters

library(mapview)
mapviewOptions(fgb=FALSE)
library(sf)
library(glue)
library(tidyverse)


# Get the Data ------------------------------------------------------------

# fire perimeters can be found here
# https://frap.fire.ca.gov/media/ly2jyr4j/fire21_2.zip
# unzip and check layers
download.file("https://frap.fire.ca.gov/media/ly2jyr4j/fire21_2.zip", destfile = "data_raw/fire21_2.zip")
unzip("data_raw/fire21_2.zip", exdir = "data_raw")

# check layers
st_layers("data_raw/fire21_2.gdb/")

# fire perimeters
firep21 <- st_read("data_raw/fire21_2.gdb/", "firep21_2")
firep21 %>% filter(FIRE_NAME=="CALDOR") %>% mapview()

# save out
# save(firep21, file="data_raw/calfire_perimeters_2021.rda")
