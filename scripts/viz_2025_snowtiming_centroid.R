# snow timing and centroid

# Libraries ---------------------------------------------------------------

needed <- c("terra","sf","tigris","exactextractr","dplyr","tidyr","lubridate","ggplot2","scales","broom")
library(pacman)
pacman::p_load(char = needed)
options(tigris_class = "sf", tigris_use_cache = TRUE)

# Download Point Climate Data ---------------------------------------------

# this is cool but only uses points: https://julianselke.github.io/TerraclimateR/
# remotes::install_github("julianselke/TerraclimateR")
# library(TerraclimateR)

# Parameters --------------------------------------------------------------

state <- "CA"
county_name <- "Trinity"
start_year <- 1980
end_year   <- 2024
wy_start_month <- 10    # water year starts Oct 1
swe_threshold_frac <- 0.5  # depletion threshold (50% of max)
outdir <- "data_terraclimate"
dir.create(outdir, showWarnings = FALSE)

# Get County Polygon ----------

cnty <- counties(state = state, cb = TRUE) %>%
  filter(NAME == county_name) %>%
  st_transform(4326)  # keep lon/lat for TerraClimate convenience

if(nrow(cnty) == 0) stop("County polygon not found. Check county_name/state.")

# bounding box to limit downloads

bb <- st_bbox(st_buffer(cnty, 0.1))
plot(cnty$geometry)

# Download From ClimateR --------------------------------------------------

# remotes::install_github("mikejohnson51/climateR")
library(climateR)
tst <- climateR::getCABCM(cnty, varname = "run", startDate = "2010-01-01")
tst[[1]]

# Download TerraClimate SWE from official THREDDS ----

# base url: each year has its own NetCDF
base_url <- "https://climate.northwestknowledge.net/TERRACLIMATE/TerraClimate_swe_"
years <- seq(start_year, end_year)

nc_files <- file.path(outdir, paste0("TerraClimate_swe_", years, ".nc"))

for (i in seq_along(years)) {
  y <- years[i]
  dest <- nc_files[i]
  if (!file.exists(dest)) {
    url <- paste0(base_url, y, ".nc")
    message("Downloading ", url)
    try(utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE), silent = TRUE)
  }
}

# confirm at least one file downloaded
stopifnot(any(file.exists(nc_files)))

# ---- 3. Stack all monthly rasters into one SpatRaster

ras_list <- lapply(nc_files[file.exists(nc_files)], rast)
swe_all <- do.call(c, ras_list)
# Add time dimension
tc_time <- seq.Date(as.Date(sprintf("%d-01-01", start_year)), as.Date(sprintf("%d-12-01", end_year)), by = "month")
if (nlyr(swe_all) > length(tc_time)) tc_time <- tc_time[1:nlyr(swe_all)]
names(swe_all) <- format(tc_time, "swe_%Y_%m")

# ---- 4. Crop/mask to Trinity County ----
swe_crop <- crop(swe_all, vect(bb))
swe_mask <- mask(swe_crop, vect(cnty))

# ---- 5. Extract county-mean SWE ----
vals <- exact_extract(swe_mask, cnty, 'weighted_mean')
if (is.list(vals)) vals <- vals[[1]]

df_ts <- tibble(
  date = tc_time,
  swe_mm = as.numeric(vals)
) |> filter(!is.na(swe_mm))

# ---- 6. Compute water-year metrics and centroid timing ----
df_ts <- df_ts |>
  mutate(month = month(date),
         year = year(date),
         water_year = if_else(month >= wy_start_month, year + 1L, year),
         month_mid_doy = yday(as.Date(paste0(year, "-", month, "-15"))))

compute_wy_metrics <- function(df_wy) {
  s <- df_wy$swe_mm
  d <- df_wy$date
  if (all(is.na(s))) return(tibble(max_swe = NA, date_max = as.Date(NA), depletion_date = as.Date(NA), centroid_doy = NA))
  max_swe <- max(s, na.rm = TRUE)
  date_max <- d[which.max(s)]
  thr <- swe_threshold_frac * max_swe
  after_max <- which(d >= date_max)
  depletion_date <- {
    idx <- which(s[after_max] <= thr)[1]
    if (length(idx)) d[after_max[idx]] else as.Date(NA)
  }
  ds <- pmax(0, -diff(s))
  centroid_doy <- if (sum(ds, na.rm = TRUE) == 0) NA else {
    mids <- df_wy$month_mid_doy[-1]
    sum(mids * ds, na.rm = TRUE) / sum(ds, na.rm = TRUE)
  }
  tibble(max_swe, date_max, depletion_date, centroid_doy)
}

wy_metrics <- df_ts |>
  group_by(water_year) |>
  group_modify(~ compute_wy_metrics(.x)) |>
  ungroup() |>
  mutate(centroid_date = as.Date(paste0(water_year - 1, "-01-01")) + round(centroid_doy) - 1)

# ---- 7. Compute centroid trend (days/decade) ----
fit <- lm(centroid_doy ~ water_year, data = wy_metrics)
slope_days_per_decade <- coef(fit)[2] * 10
cat(sprintf("Centroid trend: %.2f days/decade\n", slope_days_per_decade))

# ---- 8. Seasonal SWE envelopes for historic vs current ----
df_ts <- df_ts |>
  mutate(period = if_else(water_year <= 2000, "Historic (1980–2000)", "Current (2001–2024)"),
         month_in_wy = ((month - wy_start_month) %% 12) + 1)

seasonal_summary <- df_ts |>
  group_by(period, month_in_wy) |>
  summarise(
    median_swe = median(swe_mm, na.rm = TRUE),
    p25 = quantile(swe_mm, 0.25, na.rm = TRUE),
    p75 = quantile(swe_mm, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(seasonal_summary, aes(x = month_in_wy)) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = period), alpha = 0.25) +
  geom_line(aes(y = median_swe, color = period), size = 1) +
  scale_x_continuous(breaks = 1:12, labels = month.abb[c(wy_start_month:12, 1:(wy_start_month - 1))]) +
  labs(title = "Trinity County SWE seasonal envelope",
       y = "Mean SWE (mm)", x = paste0("Water year months (start = ", month.abb[wy_start_month], ")")) +
  theme_minimal()

# ---- 9. Optional: Map slope (for single county) ----
cnty$slope_days_decade <- slope_days_per_decade
plot(cnty["slope_days_decade"], main = "Change in centroid timing (days/decade)")
