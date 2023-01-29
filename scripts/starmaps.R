# star maps in R!
# from this great post: https://dieghernan.github.io/202301_star-map-R/

# Spatial manipulation
library(sf)
library(s2)
library(nominatimlite)

## Wrange data and dates
library(dplyr)
library(lubridate)
library(lutz)

## Visualization
library(ggplot2)
library(ggfx)
library(ggshadow)


# Helper functions --------------------------------------------------------

load_celestial <- function(filename,
                           url = "https://cdn.jsdelivr.net/gh/dieghernan/celestial_data@main/data/",
                           cachedir = tempdir()) {
  if (!dir.exists(cachedir)) {
    stop(
      "Please create ",
      path.expand(cachedir),
      " directory",
      "first"
    )
  }

  url <- file.path(url, filename)
  local_path <- file.path(cachedir, filename)


  if (!file.exists(local_path)) {
    download.file(url, local_path, mode = "wb", quiet = TRUE)
  }

  celestial <- sf::st_read(local_path, quiet = TRUE)

  return(celestial)
}

pretty_lonlat <- function(x, type, accuracy = 2) {
  positive <- x >= 0

  # Decompose
  x <- abs(x)
  D <- as.integer(x)
  m <- (x - D) * 60
  M <- as.integer(m)
  S <- round((m - M) * 60, accuracy)

  # Get label
  if (type == "lon") {
    lab <- ifelse(positive > 0, "E", "W")
  } else {
    lab <- ifelse(positive > 0, "N", "S")
  }


  # Compose
  label <- paste0(D, "\u00b0 ", M, "' ", S, '\" ', lab)
  return(label)
}


# Derive rotation degrees of the projection given a date and a longitude
get_mst <- function(dt, lng) {
  desired_date_utc <- lubridate::with_tz(dt, "UTC")


  yr <- lubridate::year(desired_date_utc)
  mo <- lubridate::month(desired_date_utc)
  dy <- lubridate::day(desired_date_utc)
  h <- lubridate::hour(desired_date_utc)
  m <- lubridate::minute(desired_date_utc)
  s <- lubridate::second(desired_date_utc)

  if ((mo == 1) || (mo == 2)) {
    yr <- yr - 1
    mo <- mo + 12
  }

  # Adjust times before Gregorian Calendar
  # See https://squarewidget.com/julian-day/
  if (lubridate::as_date(dt) > as.Date("1582-10-14")) {
    a <- floor(yr / 100)
    b <- 2 - a + floor(a / 4)
  } else {
    b <- 0
  }
  c <- floor(365.25 * yr)
  d <- floor(30.6001 * (mo + 1))

  # days since J2000.0
  jd <- b + c + d - 730550.5 + dy + (h + m / 60 + s / 3600) / 24
  jt <- jd / 36525

  # Rotation
  mst <- 280.46061837 + 360.98564736629 * jd +
    0.000387933 * jt^2 - jt^3 / 38710000.0 + lng

  # Modulo 360 degrees
  mst <- mst %% 360

  return(mst)
}

# Cut a sf object with a buffer using spherical s2 geoms
# Optionally, project and flip
sf_spherical_cut <- function(x, the_buff, the_crs = sf::st_crs(x), flip = NULL) {
  # Get geometry type
  geomtype <- unique(gsub("MULTI", "", sf::st_geometry_type(x)))[1]

  # Keep the data frame, s2 drops it
  the_df <- sf::st_drop_geometry(x)
  the_geom <- sf::st_geometry(x)
  # Convert to s2 if needed
  if (!inherits(the_buff, "s2_geography")) {
    the_buff <- sf::st_as_s2(the_buff)
  }

  the_cut <- the_geom %>%
    # Cut with s2
    sf::st_as_s2() %>%
    s2::s2_intersection(the_buff) %>%
    # Back to sf and add the df
    sf::st_as_sfc() %>%
    sf::st_sf(the_df, geometry = .) %>%
    dplyr::filter(!sf::st_is_empty(.)) %>%
    sf::st_transform(crs = the_crs)

  # If it is not POINT filter by valid and non-empty
  # This if for performance
  if (!geomtype == "POINT") {
    # If any is GEOMETRYCOLLECTION extract the right value
    if (any(sf::st_geometry_type(the_cut) == "GEOMETRYCOLLECTION")) {
      the_cut <- the_cut %>%
        sf::st_collection_extract(type = geomtype, warn = FALSE)
    }

    the_cut <- the_cut %>%
      dplyr::filter(!is.na(sf::st_is_valid(.)))
  }

  if (!is.null(flip)) {
    the_cut <- the_cut %>%
      dplyr::mutate(geometry = geometry * flip) %>%
      sf::st_set_crs(the_crs)
  }

  return(the_cut)
}


# Data --------------------------------------------------------------------

# Inputs
desired_place <- "Colfax, California"

# We are not using yet the timezone
desired_date <- make_datetime(
  year = 2015,
  month = 9,
  day = 22,
  hour = 3,
  min = 45
)

# Geocode place with nominatimlite
desired_place_geo <- geo_lite(desired_place, full_results = TRUE)

desired_place_geo %>%
  select(address, lat, lon)

# And get the coordinates
desired_loc <- desired_place_geo %>%
  select(lat, lon) %>%
  unlist()

desired_loc

# check the desired date
desired_date

# Get tz
get_tz <- tz_lookup_coords(desired_loc[1], desired_loc[2], warn = FALSE)

get_tz

# Force it to be local time
desired_date_tz <- force_tz(desired_date, get_tz)

desired_date_tz

# time zone munging if needed:
#as_datetime(paste(as.Date(desired_date_tz), "22:00:00"), tz = "UTC")
#> [1] "2015-09-22 22:00:00 UTC"

# That would really correspond to 10:00
#as_datetime(paste(as.Date(desired_date_tz), "22:00:00"), tz = "UTC") %>%
#  with_tz(get_tz)


# Setup -------------------------------------------------------------------

# Get the rotation and prepare buffer and projection

# Get right degrees
lon_prj <- get_mst(desired_date_tz, desired_loc[2])
lat_prj <- desired_loc[1]

c(lon_prj, lat_prj)

# Create proj4string w/ Airy projection
target_crs <- paste0("+proj=airy +x_0=0 +y_0=0 +lon_0=", lon_prj, " +lat_0=", lat_prj)


target_crs

# We need to flip celestial objects to get the impression of see from the Earth
# to the sky, instead of from the sky to the Earth
# https://stackoverflow.com/a/75064359/7877917
# Flip matrix for affine transformation
flip_matrix <- matrix(c(-1, 0, 0, 1), 2, 2)

# And create an s2 buffer of the visible hemisphere at the given location
hemisphere_s2 <- s2_buffer_cells(
  as_s2_geography(
    paste0("POINT(", lon_prj, " ", lat_prj, ")")
  ),
  9800000,
  max_cells = 5000
)

# This one is for plotting
hemisphere_sf <- hemisphere_s2 %>%
  st_as_sf() %>%
  st_transform(crs = target_crs) %>%
  st_make_valid()


# Get Celestial Data ------------------------------------------------------

mw <- load_celestial("mw.min.geojson")

# Add colors to MW to use on fill
cols <- colorRampPalette(c("white", "yellow"))(5)
mw$fill <- factor(cols, levels = cols)

ggplot(mw) +
  geom_sf(aes(fill = fill)) +
  scale_fill_identity()

# clean up
# Cut to buffer
mw_end <- sf_spherical_cut(mw,
                           the_buff = hemisphere_s2,
                           # Change the crs
                           the_crs = target_crs,
                           flip = flip_matrix
)


ggplot(mw_end) +
  geom_sf(aes(fill = fill)) +
  hrbrthemes::theme_ft_rc()+
  scale_fill_identity()


# Constellations ----------------------------------------------------------

const <- load_celestial("constellations.lines.min.geojson")

ggplot(const) +
  geom_sf() +
  hrbrthemes::theme_ft_rc()+
  coord_sf(expand = FALSE)


# Cut ---------------------------------------------------------------------

# Cut to buffer
const_end <- sf_spherical_cut(const,
                              the_buff = hemisphere_s2,
                              # Change the crs
                              the_crs = target_crs,
                              flip = flip_matrix
)

ggplot(const_end) +
  geom_sf() +
  hrbrthemes::theme_ft_rc()+
  coord_sf(expand = FALSE)


# Stars -------------------------------------------------------------------

stars <- load_celestial("stars.6.min.geojson")

ggplot(stars) +
  # We use relative brightness (br) as aes
  geom_sf(aes(size = br, alpha = br), color="skyblue", shape = 16) +
  scale_size_continuous(range = c(0.5, 6)) +
  scale_alpha_continuous(range = c(0.1, 0.8)) +
  coord_sf(expand = FALSE)+
  hrbrthemes::theme_ft_rc()

# cut to buffer
stars_end <- sf_spherical_cut(stars,
                              the_buff = hemisphere_s2,
                              # Change the crs
                              the_crs = target_crs,
                              flip = flip_matrix
)

ggplot(stars_end) +
  # We use relative brightness (br) as aes
  geom_sf(aes(size = br, alpha = br), color="skyblue", shape = 16) +
  scale_size_continuous(range = c(0.5, 6)) +
  scale_alpha_continuous(range = c(0.1, 0.8))+
  hrbrthemes::theme_ft_rc()



# Add Graticules ----------------------------------------------------------

grat <- st_graticule(
  ndiscr = 5000,
  lat = seq(-90, 90, 10),
  lon = seq(-180, 180, 30)
)

ggplot(grat) +
  geom_sf() +
  coord_sf(expand = FALSE)


# Cut to buffer, we dont flip this one (it is not an object of the space)
grat_end <- sf_spherical_cut(
  x = grat,
  the_buff = hemisphere_s2,
  # Change the crs
  the_crs = target_crs
)


ggplot(grat_end) +
  geom_sf() +
  coord_sf(expand = FALSE)


# Finalize Viz ------------------------------------------------------------

lat_lab <- pretty_lonlat(desired_loc[1], type = "lat")
lon_lab <- pretty_lonlat(desired_loc[2], type = "lon")

pretty_labs <- paste(lat_lab, "/", lon_lab)

cat(pretty_labs)

# Create final caption to put on bottom
pretty_time <- paste(
  # Pretty Day
  scales::label_date(
    format = "%d %b %Y",
    locale = "en"
  )(desired_date_tz),
  # Pretty Hour
  format(desired_date_tz, format = "%H:%M", usetz = TRUE)
)

cat(pretty_time)

# Our final caption
caption <- toupper(paste0(
  "Star Map\n",
  desired_place, "\n",
  pretty_time, "\n",
  pretty_labs
))

cat(caption)

# Prepare MULTILINESTRING
const_end_lines <- const_end %>%
  st_cast("MULTILINESTRING") %>%
  st_coordinates() %>%
  as.data.frame()

# the plot
ggplot() +
  # Graticules
  geom_sf(data = grat_end, color = "grey60", linewidth = 0.25, alpha = 0.3) +
  # A blurry Milky Way
  with_blur(
    geom_sf(
      data = mw_end, aes(fill = fill), alpha = 0.1, color = NA,
      show.legend = FALSE
    ),
    sigma = 8
  ) +
  scale_fill_identity() +
  # Glowing stars
  geom_glowpoint(
    data = stars_end, aes(
      alpha = br, size =
        br, geometry = geometry
    ),
    color = "white", show.legend = FALSE, stat = "sf_coordinates"
  ) +
  scale_size_continuous(range = c(0.05, 0.75)) +
  scale_alpha_continuous(range = c(0.1, 0.5)) +
  # Glowing constellations
  geom_glowpath(
    data = const_end_lines, aes(X, Y, group = interaction(L1, L2)),
    color = "white", size = 0.5, alpha = 0.8, shadowsize = 0.4, shadowalpha = 0.01,
    shadowcolor = "white", linejoin = "round", lineend = "round"
  ) +
  # Border of the sphere
  geom_sf(data = hemisphere_sf, fill = NA, color = "white", linewidth = 1.25) +
  # Caption
  labs(caption = caption, y="", x="") +
  # And end with theming
  theme_void() +
  #theme(
    #text = element_text(colour = "white"),
    #panel.border = element_blank(),
    #plot.background = element_rect(fill = "#191d29", color = "#191d29"),
    #plot.margin = margin(20, 20, 20, 20),
    #plot.caption = element_text(
    #  hjust = 0.5, face = "bold",
    #  size = rel(1),
    #  lineheight = rel(1.2),
    #  margin = margin(t = 40, b = 20)
    #)
  #) +
  hrbrthemes::theme_ft_rc(base_family = "Roboto Slab")
