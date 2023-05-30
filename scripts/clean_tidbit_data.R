# clean temperature data from excel csv

# LIBRARIES ---------------------------------------------------

library(tidyverse)
library(janitor)


# IMPORT DATA --------------------------------------------------

dat <- read_csv("~/Downloads/ibutton_151_31B342_25May2023_Celsius.csv", skip = 14)

# CLEAN DATA ---------------------------------------------------

# clean names
dat <- clean_names(dat)

# fix datetime
dat$date_time <- mdy_hms(dat$date_time)
# add site
dat$site <- "31B342"

dat <- dat %>%
  rename(temp_C = value)

# check for missing/constant (only if needed)
summary(dat)
dat <- janitor::remove_constant(dat)

# add day
dat$yday <- yday(dat$date_time)
dat$mday <- day(dat$date_time)
dat$month <- month(dat$date_time)

head(dat)

# SUMMARIZE DATA ----------------------------------------------------------

library(timetk)

# MEAN
dat %>%
  group_by(site) %>%
  summarize_by_time(.date_var = date_time,
    .by  = "day",
    mean = mean(temp_C),
    max = max(temp_C),
    min = min(temp_C)
  ) %>%
  plot_time_series(date_time, mean, .interactive = FALSE, .y_intercept = 0)

ggsave("figs/tst_tidbit_plot.png", width = 8, height = 5, units = "in", dpi=300)

# MAX
dat %>%
  group_by(site) %>%
  summarize_by_time(.date_var = date_time,
                    .by  = "day",
                    mean = mean(temp_C),
                    max = max(temp_C),
                    min = min(temp_C)
  ) %>%
plot_time_series(date_time, max, .interactive = FALSE, .y_intercept = 0)
