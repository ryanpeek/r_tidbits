# hydroloom

library(hydroloom)
library(dplyr)
library(sf)
library(nhdplusTools)

dir <- download_nhd(nhdplusTools_data_dir(),
                    hu_list = "1908",
                    download_files =  T)
fs <- list.files(dir, pattern = ".*1908.*.gdb", full.names = TRUE)
fs <- list.files("~/Downloads/NHD_H_1908_HU4_GDB/", pattern = ".*1908.*.gdb", full.names = TRUE)
fs

# Dropping Z and M and making sure everything is LINESTRING helps later.
fl <- sf::st_cast(sf::st_zm(sf::read_sf(fs, "NHDFlowline"), "LINESTRING"))

table(fl$ftype)

# remove any coastal (ftype 566) features
fl <- filter(fl, ftype != 566)

# this is a seed point near the outlet of a watershed to consider.
point <- sf::st_sfc(sf::st_point(c(-147.911472, 64.796633)),
                    crs = 4269)

# now we get the line our point is along.
i <- index_points_to_lines(fl, point)

# Let's see what we have.
sub <- fl[fl$permanent_identifier == i$permanent_identifier, ]

mapview::mapview(list(sub, point))


#remove coastal and make terminals go to an empty id.
flow_table <- sf::read_sf(fs, "NHDFlow") |>
  filter(from_permanent_identifier %in% fl$permanent_identifier) |>
  mutate(to_permanent_identifier =
           ifelse(!to_permanent_identifier %in% from_permanent_identifier,
                  "",
                  to_permanent_identifier))

# Remove loops found in navigate network dfs
remove <- hydroloom::check_hy_graph(flow_table,loop_check = TRUE)

# this is naive and these removals would normally be reviewed.
flow_table <- flow_table |>
  mutate(row = 1:n()) #|>
  filter(!row %in% remove$row)

#Error in `filter()`:
#  ℹ In argument: `!row %in% remove$row`.
#Caused by error in `remove$row`:
#  ! $ operator is invalid for atomic vectors

down <- navigate_network_dfs(flow_table, sub$permanent_identifier, direction = "down")
up <- navigate_network_dfs(flow_table, sub$permanent_identifier, direction = "up")

subdown <- fl[fl$permanent_identifier %in% unique(unlist(down)),]
subup <- fl[fl$permanent_identifier %in% unique(unlist(up)),]

map_image <- "flow-table-fig.jpeg"
map <- mapview::mapview(list(subup, subdown, point))
mapview::mapviewOptions(fgb = FALSE)
mapview::mapshot(map, file = map_image)
knitr::include_graphics(map_image)
