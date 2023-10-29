# dealing with excel files

# libraries --------------------------------------
library(tidyverse)
library(janitor)
library(glue)
library(readxl)

# read in data -----------------------------------

dat <- readxl::read_xlsx("~/Downloads/McCloud_Length_to_Date.xlsx", sheet = 3) |>
  # this makes the column names more reasonable to work with
  janitor::clean_names()

# note, it makes all the additional FL data as "x10, x11, etc"

# OK! Data is in...but looks like all the FL data needs to be transposed
# But we want to keep it associated with date...what a mess!
# we can use a few options here...

# clean data ------------------------------------

# first split out the good data that we can deal with separately:
# I'm using the native R pipe here ( |> ), it works the same way a %>% pipe works, passing data from the left to the next function on the right

# select columns we want to set aside for now:
metadat <- dat |> select(date:notes)

# now let's make a df that just has date and all the FL data
fl_data <- dat |>
  # filter to only data that has FL
  filter(match == "Y") |>
  select(date, fl:last_col())

# ok, now we want to make this data not terrible
# let's try to pivot the data from wide to long

fl_data_long <- tidyr::pivot_longer(fl_data, cols = c(fl:x63), names_to = "fl_id", values_to = "fl", values_drop_na = TRUE) |>
  # delete the FL id column since it's not useful as is
  select(-fl_id)

# If you do need a unique FL id (associated with each date):
fl_data_long <- fl_data_long |>
  group_by(date) %>%
  mutate(fl_id = seq(n()))
# this is actually a good check to have against the "total_ds" col

# great...seems good? Now join back to original dataset
fl_full_data <- left_join(metadat, fl_data_long) |>
  # let's put fl cols in a better spot
  relocate(starts_with("fl"), .after = live_release)

# one last little thing...there's a bunch of zeros from somewhere, let's remove
fl_full_data <- fl_full_data |> filter(fl != 0) |>
  # convert to date for easier plotting
  mutate(date = as.Date(date))

# THE BEST PART VISUALIZATION!! ----------------------

library(hrbrthemes) # love this for great themes

# quick check
summary(fl_full_data)

# scatter plot
(g1 <- ggplot() +
    geom_jitter(data=fl_full_data,
               aes(x=date, y=fl, fill=date), size=3,
               alpha=0.8, pch=21, show.legend = FALSE) +
    scale_fill_viridis_c("Date", option = "B", trans="date") +
    #scale_fill_viridis_c(breaks=as.numeric(lab_dates), option = "B", labels=lab_dates) +
    theme_ipsum_rc() +
    labs(x="", y="Fork length (mm)",
         title="McCloud Fishies", subtitle="FL by date"))

# cool!

# add a loess or gam trendline?
(g1_trend <- g1 +
  geom_smooth(data=fl_full_data,
              aes(x=date, y=fl), method = "loess", color="gray"))
g1 +
  geom_smooth(data=fl_full_data,
              aes(x=date, y=fl), method = "gam", color="gray")

# histogram of FL
(g2 <- ggplot() +
    geom_histogram(data=fl_full_data, aes(x=fl), binwidth = 2) +
    theme_ipsum_rc() +
    labs(x="Fork length (mm)", subtitle = "FL frequency")
  )


# Make magic with patchwork -----------------------------------------------

library(patchwork)

# mash plots together for all the data...
g1_trend / g2 + patchwork::plot_layout(heights = c(3, 1)) & theme(plot.margin = margin(2, 2, 5, 5, "pt"))

ggsave(filename = "~/Downloads/plot_of_fishies_fl_data.png",
       width = 11, height = 8, dpi=300)
