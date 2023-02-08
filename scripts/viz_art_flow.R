# flow field art
# https://github.com/cj-holmes/flow-field-art/blob/main/README.Rmd


# Function ----------------------------------------------------------------

#' Get the coords of a tapered flow line polygon
#'
#' The x_start and y_start will be used to index columns and rows of the angle_matrix
#' This means y values from a conventional Cartesian coordinate system will be reversed as y = 1 will be row 1 of the matrix
#'
#' @param x_start x starting point of flow line on angle_matrix
#' @param y_start y starting point of flow line on angle_matrix
#' @param step_length step length
#' @param n_steps number of steps
#' @param angle_matrix matrix of angles (the field)
#' @param taper_min min value of taper size
#' @param taper_max max value of taper size
ff_polys <- function(
    x_start,
    y_start,
    step_length,
    n_steps,
    angle_matrix,
    taper_max,
    taper_min){

  # Initialise vectors with the starting x and y values filled with NAs
  out_x <- c(x_start, rep(NA, n_steps))
  out_y <- c(y_start, rep(NA, n_steps))

  # If the starting point is outside the angle_matrix dimensions, return NULL
  if(x_start > ncol(angle_matrix) |
     x_start < 1 |
     y_start > nrow(angle_matrix) |
     y_start < 1){
    return(NULL)
  }

  # Loop through each step as we travel across the angle matrix
  for(i in 1:n_steps){

    # Get the angle of the nearest flow field point where we are for this iteration
    a <- angle_matrix[round(out_y[i]), round(out_x[i])]

    # Compute how far to move in x and y for the given angle and step_length
    step_x <- cos(a*(pi/180))*step_length
    step_y <- sin(a*(pi/180))*step_length

    # Add the distance in x and y to the current location
    next_x <- out_x[i] + step_x
    next_y <- out_y[i] + step_y

    # If the next point in the path sits outside the angle matrix, stop iterating along the path
    if(next_x > ncol(angle_matrix) |
       next_x < 1 |
       next_y > nrow(angle_matrix) |
       next_y < 1){
      break
    }

    # Append the new x and y location to the output
    # (ready to be used as the starting point for the next step iteration)
    out_x[i+1] <- next_x
    out_y[i+1] <- next_y
  }

  # Return tibble of the x, y, paths
  # The polygon goes out along x and then back along rev(x)
  # The y values have a taper added to them on the way out (along x)
  # and then have a reverse taper subtracted from them on the way back (along rev(x))
  tibble(x = c(out_x, rev(out_x)),
         y = c(out_y + seq(taper_min, taper_max, l=length(out_y)),
               rev(out_y) - seq(taper_max, taper_min, l=length(out_y)))) |>
    # Finally remove any NA entries from the path where the flow line got to the
    # edge of the angle_matrix and we stopped iterating
    filter(!is.na(x), !is.na(y))
}


# Read in Image -----------------------------------------------------------
library(tidyverse)
library(magick)

img <- image_read('data_raw/IMG_5951.png')
img

### Process image
# Define some parameters for imahe size and the number of flow lines wanted
# Resize image, greyscale, blur, flip and convert to matrix rescaled to angle range (0 to 180 degrees in this case)

x_side <- 400
n_flow_lines <- 5000
m <-
  img |>
  image_resize(paste0(x_side, "x")) |>
  image_convert(colorspace = "gray") |>
  image_flip() |>
  image_despeckle(2) |>
  image_blur(radius = 2, sigma=2) |>
  image_raster(tidy = FALSE) |>
  col2rgb() |>
  magrittr::extract(1,) |>
  matrix(ncol = x_side, byrow = TRUE) |>
  scales::rescale(c(0, 180))


# Generate Flow Lines -----------------------------------------------------

set.seed(1234)
starting_params <-
  tibble(x_start = runif(n_flow_lines, 1, ncol(m)),
         y_start = runif(n_flow_lines, 1, nrow(m))) |>
  mutate(id = row_number(),
         step_length = 1,
         n_steps = 300,
         taper_max = 5,
         taper_min = 0)
flow_field_coords <-
  starting_params |>
  mutate(paths = pmap(
    .l = list(x_start = x_start,
              y_start = y_start,
              step_length = step_length,
              n_steps = n_steps,
              taper_max = taper_max,
              taper_min = taper_min),
    .f = ff_polys,
    angle_matrix = m)) |>
  unnest(cols=paths)


# Plot! -------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = flow_field_coords,
    aes(x, y, group = id,
        fill = sqrt((nrow(m)/2 - y_start)^2 + (ncol(m)/2 - x_start)^2)),
    col = NA,
    alpha = 0.4)+
  coord_equal()+
  #scale_fill_viridis_c(option = "turbo", direction = -1)+
  scale_fill_viridis_c(option = "mako", direction = -1)+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black", color = NA),
    legend.position = "")


# Save --------------------------------------------------------------------

ggsave(filename = "figs/flow_field_art_wmelon.pdf", width = 10, height = 10, dpi=300)
