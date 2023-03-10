# read fit files from Garmin watch


# remotes::install_github("grimbough/FITfileR")

library(FITfileR)
library(glue)
library(fs)
library(dplyr)
library(sf)
library(mapview)
library(janitor)

# use this to check the operating system

ospath <- if (.Platform$OS.type == "windows") {
    "D://Garmin//"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }

activity_path <- glue("{ospath}Activity")

fit_ls <- fs::dir_ls(activity_path)

fit_dat <- readFitFile(fit_ls[1])

# get records
fit_records <- records(fit_dat)

# get types
listMessageTypes(fit_dat)

# get lap data
lap <- getMessagesByType(fit_dat, message_type = "lap") %>%
  clean_names() %>% remove_constant() %>%
  select(-contains("power_phase")) %>%
  st_as_sf(coords=c("start_position_long","start_position_lat"), remove=FALSE, crs=4326)

mapview(lap)

# get track data
record <- getMessagesByType(fit_dat, message_type = "record") %>%
  bind_rows() %>%
  clean_names() %>% remove_constant() %>%
  filter(!is.na(position_long)) %>%
  arrange(timestamp) %>%
  st_as_sf(coords=c("position_long","position_lat"), remove=FALSE, crs=4326)

mapview(record, zcol="heart_rate", color=NULL)
