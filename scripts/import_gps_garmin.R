# download kml or Pts from GPS


# Libraries ---------------------------------------------------------------

library(glue)
library(fs)
library(dplyr)
library(sf)
library(mapview)
library(janitor)

# use this to check the operating system
source("scripts/f_get_os.R") # gets ospath

garminpath <- if (.Platform$OS.type == "windows") {
  "D://Garmin//"
} else if (.Platform$OS.type == "unix") {
  "unix"
} else {
  stop("Unknown OS")
}

# Get GPX TRACKS --------------------------------------------------------------


tracks_path <- glue("{garminpath}/GPX")

tracks_ls <- fs::dir_ls(tracks_path, glob="*.gpx")

## GREP -----

# grep a list or date
search_grep <- "2023-04-25"

(track_files <- tracks_ls[grepl(search_grep, tracks_ls)])

# check layers
st_layers(track_files[3])
trk1a <- st_read(track_files[3], "tracks") %>% select(1:4)
trk1b <- st_read(track_files[3], "track_points") %>% select(1:11)

mapview(trk1a)
mapview(trk1a) + mapview(trk1b, col.regions="orange", cex=2)


## Save Out ----------------------------------------------------------------

trk_title <- "KWA"

fs::dir_create(glue("{get_ospath()}/Downloads/kmls"))

# write TRACK
st_write(trk1a, dsn = glue("{get_ospath()}/Downloads/kmls/{search_grep}_{trk_title}_track.kml"), layer = glue("{trk_title}_track"), delete_dsn = TRUE)

# write TRACK points
#st_write(trk1b, dsn = glue("{get_ospath()}/Downloads/kmls/{search_grep}_{trk_title}_track_pts.shp"), layer = glue("{trk_title}_track_pts"), delete_dsn = TRUE)

# add points as XY
trk1b <- trk1b %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>%
  relocate(c(X,Y), .before=track_fid)

st_write(trk1b, dsn = glue("{get_ospath()}/Downloads/kmls/{search_grep}_{trk_title}_track_pts.csv"), delete_dsn = TRUE)

# Get GPX WAYPOINTS --------------------------------------------------------------

# grep by a date (diff order)
pts_search_grep <- "Waypoints_25-APR-23"

(pts_files <- tracks_ls[grepl(pts_search_grep, tracks_ls)])

# check layers
st_layers(pts_files[1])
pts <- st_read(pts_files[1], "waypoints") %>% select(1:11)

# add points as XY
pts <- pts %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>%
  relocate(c(X,Y), .before=ele)

mapview(pts)


## Save Out ----------------------------------------------------------------

pts_title <- "KWA"

fs::dir_create(glue("{get_ospath()}/Downloads/kmls"))

# write PTS as KML
st_write(pts, dsn = glue("{get_ospath()}/Downloads/kmls/{search_grep}_{trk_title}_points.kml"), layer = glue("{trk_title}_pts"), delete_dsn = TRUE)

# write WAYPOINTS
st_write(pts, dsn = glue("{get_ospath()}/Downloads/kmls/{search_grep}_{trk_title}_points.csv"), delete_dsn = TRUE)



# Get Most Recent Files ---------------------------------------------------

#
# # get file info to identify most recent
# files_info <- fs::dir_info("data_raw",
#                            regexp = "nfa_updated")
#
# # get most recent file only if multiple exists
# (file_recent <- files_info %>%
#     select(path, modification_time) %>%
#     arrange(modification_time) %>%
#     slice_max(order_by = modification_time))
#
# # remove any other files
# files_info %>% filter(path!=file_recent$path) %>%
#   pull(path) %>%
#   fs::file_delete()
#
# # read in most recent
# df <- read_csv(glue("{file_recent$path}"))
#
#
# # pull in layers
# rb1 <- st_read(filename, st_layers(filename)[[1]][1]) %>% st_zm()
# rb2 <- st_read(filename, st_layers(filename)[[1]][2]) %>% st_zm()
#
#
