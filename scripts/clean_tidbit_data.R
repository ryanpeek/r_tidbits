# clean temperature data from excel csv

# LIBRARIES ---------------------------------------------------

library(tidyverse)
library(janitor)
library(fs)
library(patchwork)

# IMPORT DATA --------------------------------------------------

# directory with files
files <- fs::dir_ls("~/Downloads/iButtons", glob = "*csv")
dat <- read_csv(files, skip = 14, id = "file")

# CLEAN DATA ---------------------------------------------------

# clean names
dat <- clean_names(dat)

# fix datetime
dat$date_time <- mdy_hms(dat$date_time)

# add site
dat$site <- gsub("C:/Users/RPeek/Downloads/iButtons/", "", dat$file)

dat <- dat %>%
  rename(temp_C = value)

# check for missing/constant (only if needed)
summary(dat)
dat <- janitor::remove_constant(dat)

# add day/week/month
dat$yday <- yday(dat$date_time)
dat$mday <- day(dat$date_time)
dat$week <- week(dat$date_time)
dat$month <- month(dat$date_time)

head(dat)

# SUMMARIZE DATA ----------------------------------------------------------

library(timetk)

## DAILY --------------------

dat %>%
  group_by(site) %>%
  summarize_by_time(.date_var = date_time,
    .by  = "day",
    mean = mean(temp_C),
    max = max(temp_C),
    min = min(temp_C)
  ) %>%
  ungroup() -> dat_daily
  #{. ->> dat_daily} %>% # to assign on the fly

## PLOTS ------------------------

(pday_mean <- dat_daily %>%
  plot_time_series(date_time, mean, .color_var = site,
                   .smooth = FALSE,.smooth_alpha = 0.5, .smooth_period = "1 month",
                   .interactive = FALSE, .y_intercept = 0) +
    labs(title="Mean daily temperature"))

ggsave("figs/daily_mean_ibutton_plot.png", width = 8, height = 5, units = "in", dpi=300)

# MIN
(pday_min <- dat_daily %>%
    plot_time_series(date_time, min, .color_var = site,
                     .smooth = FALSE, .smooth_alpha = 0.5, .smooth_period = "1 month",
                     .interactive = FALSE, .y_intercept = 0) +
    labs(title="Min daily temperature"))

ggsave("figs/daily_min_ibutton_plot.png", width = 8, height = 5, units = "in", dpi=300)


# MAX
(pday_max <- dat_daily %>%
  plot_time_series(date_time, max, .color_var = site,
                   .smooth = FALSE, .smooth_alpha = 0.5, .smooth_period = "1 month",
                   .interactive = FALSE, .y_intercept = 0) +
    labs(title="Max daily temperature"))

ggsave("figs/daily_max_ibutton_plot.png", width = 8, height = 5, units = "in", dpi=300)

(pday_max <- dat_daily %>%
    plot_time_series(date_time, max, .color_var = site,
                     .smooth = FALSE, .smooth_alpha = 0.5, .smooth_period = "1 month",
                     .interactive = FALSE, .y_intercept = 0) +
    labs(title="Max daily temperature") +
    guides(color="none"))


# patch together
pday_max / pday_min

ggsave("figs/daily_min_max_ibutton_plot.png", width = 8, height = 5, units = "in", dpi=300)

## WEEKLY --------------------

dat %>%
  group_by(site) %>%
  summarize_by_time(.date_var = date_time,
                    .by  = "week",
                    wk_mean = mean(temp_C),
                    wk_max = max(temp_C),
                    wk_min = min(temp_C)
  ) %>%
  ungroup() -> dat_weekly


# MAX
(pwk_max <- dat_weekly %>%
    plot_time_series(date_time, wk_max, .color_var = site,
                     .smooth = FALSE, .smooth_alpha = 0.5, .smooth_period = "1 month",
                     .interactive = FALSE, .y_intercept = 0) +
    labs(title="Max weekly temperature"))

ggsave("figs/weekly_max_ibutton_plot.png", width = 8, height = 5, units = "in", dpi=300)

(pwk_max <- dat_weekly %>%
    plot_time_series(date_time, wk_max, .color_var = site,
                     .smooth = FALSE, .smooth_alpha = 0.5, .smooth_period = "1 month",
                     .interactive = FALSE, .y_intercept = 0) +
    labs(title="Max weekly temperature") +
    guides(color="none"))

# MIN
(pwk_min <- dat_weekly %>%
    plot_time_series(date_time, wk_min, .color_var = site,
                     .smooth = FALSE, .smooth_alpha = 0.5, .smooth_period = "1 month",
                     .interactive = FALSE, .y_intercept = 0) +
    labs(title="Min weekly temperature"))

ggsave("figs/weekly_min_ibutton_plot.png", width = 8, height = 5, units = "in", dpi=300)

pwk_max / pwk_min

ggsave("figs/weekly_min_max_ibutton_plot.png", width = 8, height = 5, units = "in", dpi=300)


# Export ------------------------------------------------------------------

write_csv(dat_daily, file = "data_out/daily_ibutton_data.csv")
write_csv(dat_weekly, file = "data_out/weekly_ibutton_data.csv")
