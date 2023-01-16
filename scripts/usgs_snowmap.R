# USGS snow example
# see here, by Althea Archer
#https://waterdata.usgs.gov/blog/snow-tiles-demo/

library(qs)
library(sbtools) # used to download Sciencebase data
library(tidyverse) # used throughout
library(terra)
library(fs)

# Set up your global input folder name
input_folder_name <- "data_raw/2023_snowtiles_demo"
fs::dir_create(input_folder_name)



# Download Data -----------------------------------------------------------

# Download the snow cover index (SCI) raster
for(yy in 2001:2020){
  # if files already exist, skip download
  file_in <- sprintf("%s/MOD10A2_SCI_%s.tif", input_folder_name, yy)
  if(!file.exists(file_in)){ # if files don't exist, download
    sbtools::item_file_download(sb_id = "5f63790982ce38aaa23a3930",
                                names = sprintf("MOD10A2_SCI_%s.tif", yy),
                                destinations = file_in,
                                overwrite_file = F)
  }
}



# Read In Data ------------------------------------------------------------

# Read in SCI geotif files and convert to raster stack
sci_files <- list.files(sprintf("%s/", input_folder_name),
                        pattern = "MOD10A2_SCI",
                        full.names = T)
sci_stack <- terra::rast(sci_files)

# Calculate 20 year mean snow cover index (SCI) for each raster cell
sci_20yr_mean <- mean(sci_stack)


# Plot --------------------------------------------------------------------

# Plot the 20 year mean snow cover index data as-is
terra::plot(sci_20yr_mean, col=viridis::viridis(50))


# Summarize by County in CA -----------------------------------------------

library(spData)
library(sf)
library(scico)

# Download US State boundaries as sf object
states_shp <- spData::us_states
cnty_shp <- tigris::counties(state="CA")

# Reproject the sf object to match the projection of the raster
states_proj <- states_shp |> sf::st_transform(crs(sci_20yr_mean))

cnty_proj <- cnty_shp |> sf::st_transform(crs(sci_20yr_mean))


# Clip the raster to the states boundaries to speed up processing
sci_stack_clip <- terra::crop(x = sci_20yr_mean, y = vect(cnty_proj), mask = TRUE)

# Extract the SCI values to each state
extract_SCI_states <- terra::extract(x = sci_stack_clip, vect(cnty_proj))

# Calculate mean SCI by state
SCI_by_state <- as.data.frame(extract_SCI_states) |>
  group_by(ID) |>
  summarise(mean_20yr = mean(mean, na.rm = T))

# Left-join calculated 20-year SCI means to the US States sf object
SCI_state_level <- cnty_proj |>
  mutate(ID = row_number()) |>
  left_join(SCI_by_state, by = "ID")

# Set up theme for all maps to use, rather than default
theme_set(theme_void()+
            theme(plot.background = element_rect(fill = "#0A1927"),
                  legend.text = element_text(color = "#ffffff"),
                  legend.title = element_text(color = "#ffffff")))
# #0A1927 comes from
# scico::scico(n = 1, palette = "oslo", direction = 1, begin = 0.1, end = 0.1)

# Plot choropleth
ggplot() +
  geom_sf(data = SCI_state_level, aes( fill = mean_20yr)) +
  scico::scale_fill_scico(palette = "oslo", direction = 1, begin = 0.25)




# Make Hex ----------------------------------------------------------------

# library(geojsonsf)
# library(broom)
# library(rgeos)

# make hex tesselation of CONUS
columns <- 70
rows <- 70
hex_grid <- cnty_proj %>%
  # using the project states boundaries, make a hexagon grid that is 70 by 70 across the US
  sf::st_make_grid(n = c(columns, rows),
                   what = "polygons",
                   # if square = TRUE, then square. Otherwise hexagonal
                   square = FALSE) %>%
  sf::st_as_sf() %>%
  mutate(geometry = x) %>%
  mutate(hex = as.character(seq.int(nrow(.))))

# Map with states on top to see if the hexagons all lined up correctly
ggplot(hex_grid) +
  geom_sf(fill = "NA", color = "#999999") +
  geom_sf(data = cnty_proj |> st_as_sf(), color = "cyan", fill = "NA")


# Summarize by County Hex -------------------------------------------------

# Extract values to the hexagon grid from the masked raster
extract_SCI_hex <- terra::extract(x = sci_stack_clip, vect(hex_grid))

# Calculate the 20-year SCI means to each hexagon
SCI_by_hex <- as.data.frame(extract_SCI_hex) |>
  group_by(ID) |>
  summarise(mean_20yr = mean(mean, na.rm = T))

# Calculate 20-year SCI means and left-join to the hexagon grid sf object
SCI_hex_grid <- hex_grid |>
  mutate(ID = row_number()) |>
  left_join(SCI_by_hex, by = "ID") |>
  filter(! is.na(mean_20yr)) # delete hexagons outside the US boundaries

# Map the mean SCI values to see if the joins and calculations worked
SCI_hex_grid %>%
  ungroup() %>%
  ggplot() +
  geom_sf(aes(fill = mean_20yr),
          color = "black",
          size = 0.2) +
  scale_fill_scico("20 yr snow", palette = "oslo", direction = 1, begin = 0.20, end = 1)


# Add Snowflake -----------------------------------------------------------

SCI_hex_grid |>
  ungroup() %>%
  ggplot() +
  geom_sf(aes(fill = mean_20yr),
          color = "black",
          size = 0.2) +
  scale_fill_scico(palette = "oslo", direction = 1, begin = 0.20, end = 1) +
  ggimage::geom_image(aes(image = sprintf("%s/snowMask.png", input_folder_name),
                          x = st_coordinates(st_centroid(SCI_hex_grid))[,1],
                          y = st_coordinates(st_centroid(SCI_hex_grid))[,2]),
                      asp = 1.60, size = 0.015) +
  theme_void()+
  theme(plot.background = element_rect(fill = "#0A1927"),
        legend.text = element_text(color = "#ffffff"),
        legend.title = element_text(color = "#ffffff"))


# Make Fancy Plot and Save ------------------------------------------------

library(magick)
library(purrr)
library(cowplot)

# make canvas
library(showtext)
library(sysfonts)
library(ggimage)

# First make the main choropleth with snowflakes laid on top
hexmap <- ggplot() +
  # main hexmap
  geom_sf(data = spdf_hex_sci,
          aes(fill = mean_20yr),
          color="#25497C") +
  # snowflake overlay with centroids in "x" and "y" format for ggimage to read
  ggimage::geom_image(aes(image = sprintf("%s/snowMask.png", input_folder_name),
                          x = st_coordinates(st_centroid(spdf_hex_sci))[,1],
                          y = st_coordinates(st_centroid(spdf_hex_sci))[,2]),
                      asp = 1.60, size = 0.094) +
  scale_fill_scico(palette = "oslo", direction = 1, begin = 0.20, end = 1) +
  theme(legend.position = "none")

# Load some custom fonts and set some custom settings
font_legend <- 'Pirata One'
sysfonts::font_add_google('Pirata One')
showtext::showtext_opts(dpi = 300, regular.wt = 200, bold.wt = 700)
showtext::showtext_auto(enable = TRUE)

# For now, just using simple white for the font color
text_color <- "#ffffff"

# background filled with dark blue, extracted from scico but darker than used in the map's color ramp
canvas <- grid::rectGrob(
  x = 0, y = 0,
  width = 16, height = 9,
  gp = grid::gpar(fill = "#0A1927", alpha = 1, col = "#0A1927")
  # #0A1927 comes from
  # scico::scico(n = 1, palette = "oslo", direction = 1, begin = 0.1, end = 0.1)
)

# Make the composition using cowplot
ggdraw(ylim = c(0,1),
       xlim = c(0,1)) +
  # a background
  draw_grob(canvas,
            x = 0, y = 1,
            height = 9, width = 16,
            hjust = 0, vjust = 1) +
  # the national hex map
  draw_plot(hexmap,
            x = 0.01,
            y = 0.01,
            height = 0.95) +
  # map title
  draw_label("\'Tis the season to be snowy!",
             x = 0.05,
             y = 0.9,
             hjust = 0,
             vjust = 1,
             fontfamily = font_legend,
             color = text_color,
             size = 26)

# Use the built in scico function to extract 9 values of colors used in the scico color scale
colors <- scico::scico(n = 9, palette = "oslo", direction = 1, begin = 0.20, end = 1)

# Direct magick to the image file of the snowflake mask
snowflake <- magick::image_read(sprintf("%s/snowMask.png", input_folder_name))

# Assign text colors to the nearly-white value of the color ramp
text_color = colors[8]

# Load in USGS logo and colorize it using the color ramp
#usgs_logo <- magick::image_read(sprintf("%s/usgs_logo.png", input_folder_name)) %>%
#  magick::image_colorize(100, colors[7])

# Define some baseline values that we will use to systematically build custom legend
legend_X = 0.405 # baseline value for the legend hexagons - every other one will stagger off this
legend_X_evenNudge = 0.01 # amount to shift the evenly numbered legend pngs
legend_Y = 0.15 # baseline for bottom hex, each builds up off this distance
legend_Y_nudge = 0.035 # amount to bump the png up for each legend level

# First function here creates a staggered placement for each of the 9 snowflakes in the legend
hex_list <- purrr::map(1:9, function (x){
  # determine if odd or even number for X-positioning
  nudge_x <- function(x){
    if((x %% 2) == 0){ #if even
      return(legend_X_evenNudge)
    } else {
      return(0)
    }
  }

  # Draw image based on baseline and nudge positioning
  draw_image(snowflake |>
               magick::image_colorize(opacity = 100, color = colors[x]),
             height = 0.04,
             y = legend_Y + legend_Y_nudge*(x-1),
             x = legend_X + nudge_x(x))
})

# Second function here similarly systematically places the labels aside each legend snowflake
legend_text_list <- purrr::map(1:9, function (x){
  # determine if odd or even number for X-positioning
  nudge_x <- function(x){
    if((x %% 2) == 0){ #if even
      return(0.04)
    } else {
      return(0)
    }
  }

  # Draw text based on baseline and nudge positioning
  draw_text(sprintf("%s%%", (x-1)*10),
            y = legend_Y + legend_Y_nudge*(x-1),
            x = 0.93 - nudge_x(x),
            color = text_color,
            vjust = -1)
})

# Define the main plot
main_plot <- SCI_hex_grid |>
  ungroup() %>%
  ggplot() +
  geom_sf(aes(fill = mean_20yr),
          color = "black",
          size = 0.2) +
  scale_fill_scico(palette = "oslo", direction = 1, begin = 0.20, end = 1) +
  ggimage::geom_image(aes(image = sprintf("%s/snowMask.png", input_folder_name),
                          x = st_coordinates(st_centroid(SCI_hex_grid))[,1],
                          y = st_coordinates(st_centroid(SCI_hex_grid))[,2]),
                      asp = 1.60, size = 0.015) + #ggimage package
  theme(legend.position = "none")

ggdraw(ylim = c(0,1),
       xlim = c(0,1)) +
  # a background
  draw_grob(canvas,
            x = 0, y = 1,
            height = 9, width = 16,
            hjust = 0, vjust = 1) +
  # national hex map
  draw_plot(main_plot,
            x = 0.01, y = 0.01,
            height = 1) +
  # explainer text
  draw_label("Snow Cover Index is the fraction of time snow was\non the ground from January 1 to July 3, 2001 to 2020.\nData from: doi.org/10.5066/P9U7U5FP",
             fontfamily = sysfonts::font_add_google("Source Sans Pro"),
             x = 0.04, y = 0.115,
             size = 14,
             hjust = 0, vjust = 1,
             color = colors[6])+
  # Title
  draw_label("\'Tis the season\nto be snowy!",
             x = 0.04, y = 0.285,
             hjust = 0, vjust = 1,
             lineheight = 0.75,
             fontfamily = font_legend,
             color = text_color,
             size = 55) +
  # Legend title
  draw_label("Snow Cover Index",
             fontfamily = font_legend,
             x = 0.86, y = 0.50,
             size = 18,
             hjust = 0, vjust = 1,
             color = text_color)+
  # Legend snowflakes
  hex_list +
  # Legend lables
  legend_text_list #+
  # Add logo
  # draw_image(usgs_logo,
  #            x = 0.86, y = 0.05,
  #            width = 0.1,
  #            hjust = 0, vjust = 0,
  #            halign = 0, valign = 0)


# Save --------------------------------------------------------------------


# Save the final image in Twitter's 16 by 9 format
ggsave(sprintf("%s/snowtilesTwitter.png", input_folder_name),
       width = 16, height = 9, dpi = 300)
