# httr call to drought monitor

library(httr)
library(tidyverse)
library(lubridate)
library(glue)
library(tigris)
source("scripts/f_water_year.R")

# Set a Path Parameters ----------------------------

fips <- tigris::list_counties(state = "CA") %>%
  mutate(ca_cnty_fips = glue("06{county_code}"))
start_date <- "10/1/1999" # same time frame for all
end_date <- format(Sys.Date(), format="%m/%d/%Y")
aoi <- c(fips$ca_cnty_fips)
aoi_str <- glue_collapse(aoi, ",")
area <- "CountyStatistics"
stats_type <- "GetDroughtSeverityStatisticsByAreaPercent"
#stats_type <- "GetDSCI"
id_out <- "cnty"
statid <- 2 # categorical

# Set the Path ------------------------------------------------------------

dm_path <- glue("https://usdmdataservices.unl.edu/api/{area}/{stats_type}?aoi={aoi_str}&startdate={start_date}&enddate={end_date}&statisticsType={statid}")

# Download Data -----------------------------------------------------------

# Get info
dm_get <- GET(url=dm_path, accept("text/csv"))
dm_dat <- readr::read_csv(content(dm_get, "text"))

# Clean data ---------------------------------------------

dm_dat_clean <- dm_dat %>%
  mutate(
    # convert to percents across
    across(c(MapDate, ValidStart, ValidEnd), ymd),
    across(None:D4, ~as.numeric(.x) / 100)) %>%
  rename("date" = "MapDate", "county" = "County" ) %>%
  # pivot_longer(
  #   cols = c(None:D4),
  #   names_to = "category",
  #   values_to = "percentage"
  # ) %>%
  #filter(category != "None") %>%
  #mutate(category = factor(category)) %>%
  dplyr::select(-c(ValidStart, ValidEnd, State, StatisticFormatID, None)) %>%
  mutate(
    year = year(date),
    wyear = dataRetrieval::calcWaterYear(date),
    wyday = add_wyd(date),
    week = week(date),
    wyweek = add_wyweek(wyday)) %>%
  group_by(year) %>%
  mutate(max_week = max(wyweek)) %>% ## for var
  ungroup()

# Calc DSCI v1 ------------------------------------------------------------

# formula for categorical
# 1(D0) + 2(D1) + 3(D2) + 4(D3) + 5(D4) = DSCI

dat_dsci <- dm_dat_clean %>%
  group_by(date, wyear, wyweek, county) %>%
  summarize(dsci = 100*(1*D0 + 2*D1 + 3*D2 + 4*D3 + 5*D4))

dat_dsci_sum <- dat_dsci %>% ungroup() %>%
  group_by(county, wyear) %>%
  mutate(dsci_wysum = cumsum(dsci),
         dsci_summax = max(dsci_wysum))

dat_dsci_max_yr <- dat_dsci_sum %>% ungroup() %>%
  select(wyear, county, dsci_summax) %>%
  distinct(.keep_all = TRUE)

# plot by county
ggplot() + geom_line(data=dat_dsci_sum, aes(x=date, y=dsci_wysum, color=county), show.legend=FALSE) +
  theme_classic()

ggplot() + geom_line(data=dat_dsci_max_yr, aes(x=wyear, y=dsci_summax, color=county), show.legend=FALSE) +
  theme_classic() + facet_wrap(~county)
