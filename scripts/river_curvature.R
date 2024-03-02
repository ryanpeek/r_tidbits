# curvature of rivers?
# using road-efficiency plot here: https://spencerschien.info/post/road_directness/

# Libaries ----------------------------------------------------------------

library(osmdata)
library(nhdplusTools)
library(sf)
library(tidyverse)
library(glue)
library(tigris)
options(tigris_use_cache=TRUE)
library(conflicted)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

# Get Boundaries ----------------------------------------------------------

# this is used to get roads for a given spot, but can do for a river too
ca_cntys <- counties("CA")
cnty_sel <- ca_cntys |> filter(NAME %in% c("El Dorado", "Placer", "Nevada")) |> st_transform(4326)

cnty_sel <- ca_cntys |> st_transform(4326)

# Get OSM data ------------------------------------------------------------

# test

# make a bbox
osm_bbox <- st_bbox(st_union(cnty_sel))

# get roads
# sec_roads <- opq(osm_bbox) |>
#   add_osm_feature(key = "highway", value = "secondary") |>
#   osmdata_sf()

# get osm water
# c_water <- opq(osm_bbox) |>
#   add_osm_feature(key = 'natural', value = 'water') %>%
#   osmdata_sf()

# plot
#plot(cnty_sel$geometry)
#plot(st_geometry(c_water$osm_lines), col = 'blue', add = T)
#plot(st_geometry(sec_roads$osm_lines), col = 'gray', add = T)

# Function to get OSM -----------------------------------------------------

road_types <- c("secondary","tertiary")

# download to rdata
walk(road_types, function(x) {
  if (!file.exists(paste("data_raw/osm_", x, ".rda", sep = ""))) {
    t <- opq(osm_bbox) %>%
      add_osm_feature(key = "highway", value = x) %>%
      osmdata_sf()

    saveRDS(t, file = paste("data_raw/osm_", x, ".rda", sep = ""))
  } else {
    cat(crayon::cyan("All queries complete!"))
  }
})

# read in
d_files <- list.files("data_raw", pattern = "^osm_")

roads <- map_df(d_files, function(x) {
  cat(crayon::cyan(paste("Starting", x, "\n")))
  t <- readRDS(paste("data_raw/", x, sep = ""))
  cat(crayon::red(paste("Finished", x, "\n")))

  if (!is.null(t$osm_lines)) {
    t$osm_lines %>%
      select(geometry)
  }
})

# check
plot(cnty_sel$geometry)
plot(st_geometry(roads$geometry), col = 'gray', add = T)

# trim to just county
roads_c <- roads[cnty_sel,]
plot(cnty_sel$geometry)
plot(roads_c$geometry, col = 'gray', add = T)

# Get NHD Layers ----------------------------------------------------------

nhd_rivs <- nhdplusTools::get_nhdplus(st_union(cnty_sel))

# save out
saveRDS(nhd_rivs, "data_raw/nhd_rivs_sel_cntys.rda")

plot(st_geometry(nhd_rivs$geometry), col="blue", lwd=0.5, add=T)

library(rnaturalearth)
# Import lakes shapefile
l <- ne_download(type = "lakes", category = "physical", scale = 10)  |>
  st_as_sf(., crs = 4326)

# Filter for Great Lakes that border Wisconsin
ca_l <- l |>
  filter(name %in% c("Lake Tahoe"))

# Erase lakes from counties
cnty_sel_trim <- st_difference(cnty_sel, ca_l)

# erase rivers from lake bed
nhd_rivs_c <- st_difference(st_transform(nhd_rivs, 4326), ca_l)

# or just select only streamrivers
nhd_rivs_c <- nhd_rivs |> filter(ftype=="StreamRiver")
nhd_rivs_o <- nhd_rivs |> filter(ftype!="StreamRiver")

plot(cnty_sel_trim$geometry)
plot(ca_l$geometry, add=T, col=alpha("blue", 0.5))
plot(nhd_rivs_c$geometry, col="blue", lwd=0.5, add=T)
plot(nhd_rivs_o$geometry, col="orange", lwd=1, add=T)
plot(roads_c$geometry, col = 'gray', add = T)

# Calculate Curviness ---------------------------------------------------------

# this is setup to work via counties
all_segs <- map_df(1:nrow(cnty_sel_trim), function(c) {
  one_c <- cnty_sel_trim[c,]
  county <- one_c$NAME

  p_county <- st_intersection(nhd_rivs_c, one_c)

  temp_ <- map_df(1:nrow(p_county), function(x) {
    cat(crayon::cyan("Starting County", county, c, "NHD", x, "  "))
    t_ <- p_county[x,"geometry"]

    # Extract LINESTRINGS from GEOMETRYCOLLECTION
    if (st_geometry_type(t_) == "GEOMETRYCOLLECTION") {
      t_ <- st_collection_extract(t_,type = c("LINESTRING"))
    }

    if (st_geometry_type(t_) == "MULTILINESTRING") {
      t_ <- st_cast(t_, "LINESTRING")
    }

    # Handle LINESTRINGS
    if (st_geometry_type(t_[1,]) == "LINESTRING") {
      cat(crayon::white(paste0(st_geometry_type(t_), "\n")))

      map_df(1:nrow(t_), function(ml) {
        sub_l <- t_[ml,]
        t_points <- st_cast(sub_l, "POINT")

        dist <- st_distance(t_points[1,], t_points[nrow(t_points),])
        l_length <- st_length(sub_l)
        point_count <- nrow(t_points)

        df <- tibble(dist = dist,
                     l_length = l_length,
                     point_count = point_count,
                     county = county)
      })

    } else {
      cat(crayon::red(paste0("OTHER GEOMETRY: ", st_geometry_type(t_), "\n")))
    }
  })

  return(temp_)
})

# now calculate "curvy efficiency"
aw_summed <- all_segs %>%
  group_by("NAME" = county) %>%
  summarise(tot_dist = sum(dist, na.rm=TRUE),
    total_eff = sum(dist, na.rm = TRUE) / sum(l_length, na.rm = TRUE))


# Plot --------------------------------------------------------------------

library(colorspace)
library(viridis)
library(MetBrewer)
library(showtext)
library(ggtext)
library(scales)
library(geomtextpath)

# join data
aw_sf <- left_join(cnty_sel_trim, aw_summed)
#colpal <- hclwizard()
#col_cust <- colpal(3)

col_cust <- viridis(5)
col_cust <- rev(met.brewer("VanGogh3", n = 5)[c(1, 3, 5)])

font_add_google("Indie Flower", "idf")
font_add_google("Grechen Fuemen", "gf")
showtext_auto()

# label plot
aw_summed %>%
  arrange(total_eff) %>%
  head(5) %>%
  bind_rows(aw_summed %>%
              arrange(desc(total_eff)) %>%
              head(5)) %>%
  ggplot(aes(reorder(NAME, as.numeric(total_eff)), as.numeric(total_eff))) +
  geom_segment(aes(xend = reorder(NAME, as.numeric(total_eff)),
                   x = reorder(NAME, as.numeric(total_eff)),
                   yend = .80, y = as.numeric(total_eff)),
               linetype = 2, color = col_cust[3]) +
  geom_point(size = 12, color = col_cust[1]) +
  geom_text(aes(label = percent(as.numeric(total_eff), 0.5)),
            size = 3.5, color = "white", family = "gf") +
  scale_y_continuous(labels = function(x) percent(x, 1)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        axis.text.x = element_text(family = "idf", size = 14, angle = 20,
                                   color = col_cust[2]),
        axis.text.y = element_text(color = col_cust[2], family="gf"),
        text = element_text(family = "idf"),
        plot.title = element_textbox(family = "gf", size = 24, color = col_cust[1]),
        plot.subtitle = element_text(color = col_cust[1], size = 16),
        axis.title.y = element_textbox_simple(size = 16, halign = .5,
                                              orientation = "left",
                                              lineheight = .8,
                                              color = col_cust[1]),
        axis.title.x = element_text(size = 16, color = col_cust[1]),
        plot.caption = element_textbox_simple(color = alpha(col_cust[1], .75),
                                              size = 10,
                                              margin = margin(t = 5, b = 5)),
        plot.caption.position = "plot") +
  labs(y = "Stream Meanderness<br><span style='font-size:10pt'> (100% is a straight line)</span>",
       x = "Counties",
       title = "CA counties with the curviest and straightest streams",
       subtitle = "Counties represent the straightest to curviest",
       caption = "Data from NHD Tools." %+%
         "Analysis and graphic by R.Peek, adapted from Spencer Schien.")

# map
aw_sf  |>
  ggplot() +
  geom_sf(aes(fill = NAME), color = "white", size = .5) +
  scale_fill_manual(values = col_cust, labels = c("Curviest", "", "Straightest")) +
  theme_void() +
  theme(plot.title = element_text(family = "gf", size = 30, color = col_cust[1],
                                  margin = margin(t = 5, b = 5), hjust = .5),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(family = "gf", color = col_cust[1],
                                               size = 20, lineheight = .75,
                                               fill = alpha(col_cust[3], .25),
                                               r = unit(.05, "cm"), box.colour = col_cust[1],
                                               linetype = 1,
                                               padding = margin(l = 5,
                                                                t = 3,
                                                                b = 3,
                                                                r = 3)),
        plot.caption = element_textbox_simple(family = "gf", lineheight = .9,
                                              width = unit(7.5, "in"), size = 12,
                                              margin = margin(b = 5),
                                              color = alpha(col_cust[1], .75),
                                              fill = alpha(col_cust[3], .1),
                                              r = unit(.05, "cm"),
                                              box.colour = alpha(col_cust[1], .75),
                                              linetype = 1, linewidth = .1,
                                              padding = margin(rep(3, 4))),
        legend.position = c(.08, .25),
        legend.direction = "vertical",
        legend.title.align = -.5,
        legend.spacing = unit(10, "cm"),
        legend.text = element_text(family = "gf", size = 14)) +
  labs(title = "Winding Streams",
       subtitle = "N. Sierra counties curviest streams",
       fill = "",
       caption = glue("Stream curvature calculated as the straight-line distance between \n
                      start and end points of a stream divided by the actual length of the stream. \n
                      Data from NHD using NHDplusTools, limited to Stream/Rivers \n
                      R. Peek, adapted from Spencer Schien (@MrPecners).")) +
  guides(fill = guide_legend(label.position = "left"))

