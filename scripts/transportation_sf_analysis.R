# network analysis
# from Robin Lovelace's book: https://geocompr.robinlovelace.net/transport.html

# Libraries ---------------------------------------------------------------

library(sf)
library(dplyr)
library(spDataLarge) # install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")
library(stplanr)      # for processing geographic transport data
library(tmap)         # map making (see Chapter 9)
library(ggplot2)      # data visualization package
library(sfnetworks)   # spatial network classes and functions
library(osmdata)

# Spatial Autocorrelation -------------------------------------------------

# ‘first law’ of geography, defined by Waldo Tobler in 1970 as follows (Miller 2004):

#  `Everything is related to everything else, but near things are more related than distant things.`


# Get Some Data -----------------------------------------------------------

#sac <- osmdata::getbb("Sacramento", format_out = "sf_polygon", limit = 1)

bristol_region <- osmdata::getbb("Bristol", format_out = "sf_polygon", limit = 1)
plot(bristol_region$geometry)

zones_attr = bristol_od |>
  group_by(o) |>
  summarize(across(where(is.numeric), sum)) |>
  dplyr::rename(geo_code = o)

summary(zones_attr$geo_code %in% bristol_zones$geo_code)

zones_joined = left_join(bristol_zones, zones_attr, by = "geo_code")

zones_destinations = bristol_od |>
  group_by(d) |>
  summarize(across(where(is.numeric), sum)) |>
  dplyr::select(geo_code = d, all_dest = all)
zones_od = inner_join(zones_joined, zones_destinations, by = "geo_code")


qtm(zones_od, c("all", "all_dest")) +
  tm_layout(panel.labels = c("Origin", "Destination"))

bristol_od$Active = (bristol_od$bicycle + bristol_od$foot) /
  bristol_od$all * 100

# travel between zones: interzonal and intrazonal. Interzonal OD pairs represent travel between zones in which the destination is different from the origin.
od_intra = filter(bristol_od, o == d)
od_inter = filter(bristol_od, o != d)

# Creating centroids representing desire line start and end points.
desire_lines = od2line(od_inter, zones_od)

tm_shape(desire_lines) + tm_lines(lwd = "all", col="Active", palette = viridis::viridis(8,option = "C"))


# pick top 3 routes
desire_rail = top_n(desire_lines, n = 5, wt = train)
ncol(desire_rail)
#> [1] 9
desire_rail = line_via(desire_rail, bristol_stations)
ncol(desire_rail)


# routes
desire_lines$distance_km = as.numeric(st_length(desire_lines)) / 1000
desire_lines_short = desire_lines |>
  filter(car_driver >= 100, distance_km <= 5, distance_km >= 2.5)

# convert these desire lines into routes. This is done using the publicly available OSRM service with the stplanr functions route() and route_osrm() in the code chunk below:
routes_short = route(l = desire_lines_short,
                     route_fun = route_osrm,
                     osrm.profile = "bike")

# plot
tm_shape(desire_lines) + tm_lines(col="black", lty = 2)+
  tm_shape(routes_short) + tm_lines(col="red4", lwd=0.8)

# change routing preferences

## create route networks as an output derived from route level data, imagine a simple scenario of mode shift. Imagine that 50% of car trips between 0 to 3 km in route distance are replaced by cycling, a percentage that drops by 10 percentage points for every additional km of route distance so that 20% of car trips of 6 km are replaced by cycling and no car trips that are 8 km or longer are replaced by cycling.

uptake = function(x) {
  case_when(
    x <= 3 ~ 0.5,
    x >= 8 ~ 0,
    TRUE ~ (8 - x) / (8 - 3) * 0.5
  )
}
routes_short_scenario = routes_short |>
  mutate(uptake = uptake(distance / 1000)) |>
  mutate(bicycle = bicycle + car_driver * uptake,
         car_driver = car_driver * (1 - uptake))
sum(routes_short_scenario$bicycle) - sum(routes_short$bicycle)
#> [1] 3980


# aggregate:
route_network_scenario = overline(routes_short_scenario, attrib = "bicycle")


# sfnetwork
bristol_ways$lengths = st_length(bristol_ways)
ways_sfn = as_sfnetwork(bristol_ways)
class(ways_sfn)


# this
ways_centrality = ways_sfn |>
  activate("edges") |>
  mutate(betweenness = tidygraph::centrality_edge_betweenness(lengths))

plot(ways_centrality)
