# httr call to drought monitor

library(httr)
library(tidyverse)
library(lubridate)
library(glue)
library(janitor)
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

dm_clean <- dm_dat %>%
  mutate(
    # convert to percents across
    across(c(MapDate, ValidStart, ValidEnd), ymd),
    across(None:D4, ~as.numeric(.x) / 100)) %>%
  rename("date" = "MapDate") %>%
  # pivot_longer(
  #   cols = c(None:D4),
  #   names_to = "category",
  #   values_to = "percentage"
  # ) %>%
  #filter(category != "None") %>%
  #mutate(category = factor(category)) %>%
  dplyr::select(-c(ValidStart, ValidEnd, State, StatisticFormatID, None)) %>%
  clean_names() %>%
  mutate(
    year = year(date),
    wyear = dataRetrieval::calcWaterYear(date),
    wyday = add_wyd(date),
    mon = month(date),
    week = week(date),
    wyweek = add_wyweek(wyday)) %>%
  group_by(year) %>%
  mutate(max_week = max(wyweek)) %>% ## for var
  ungroup()


# Get a List of CDFW Regions to Join w County -----------------------------

library(sf)
library(geoarrow)

dfw <- geoarrow::read_geoparquet_sf("data_raw/dfw_regions.parquet") %>%
  #mutate(REGION=as.factor(REGION)) %>%
  janitor::clean_names()
ca_cntys <- tigris::counties(state = "CA")
ca_cntys <- st_transform(ca_cntys, st_crs(dfw))

# join and add region to cntys data
ca_dfw <- st_join(st_centroid(ca_cntys), dfw[c("region", "name")] ) %>%
  # fix san francisco
  mutate(region = case_when(
    GEOID=="06075" ~ 3L,
    TRUE ~ region),
    name = case_when(
      GEOID=="06075" ~ "North Central Region",
      TRUE ~ name),
    )

# mapview::mapview(ca_dfw, zcol="region") +
#   mapview::mapview(ca_cntys, col.regions="gray", alpha.regions=0, color="orange")+
#   mapview::mapview(dfw, zcol="region", alpha.regions=0.4)

# make simple df
ca_dfw <- ca_dfw %>% st_drop_geometry() %>%
  select(fips=GEOID, county_nm=NAME, region, region_name = name)

# Calc DSCI v1 ------------------------------------------------------------

# formula for categorical
# 1(D0) + 2(D1) + 3(D2) + 4(D3) + 5(D4) = DSCI

# group by week and calc DSCI
dsci <- dm_clean %>%
  group_by(date, wyear, wyweek, fips) %>%
  summarize(dsci = 100*(1*d0 + 2*d1 + 3*d2 + 4*d3 + 5*d4)) %>%
  mutate(mon = as.factor(month(date))) %>%
  ungroup() %>%
  left_join(ca_dfw, by="fips")

ggplot() +
  geom_violin(data=dsci, aes(x=mon, y=dsci, fill=as.factor(region)),color=NA, alpha=0.7) +
  geom_boxplot(data=dsci, aes(x=mon, y=dsci, fill=as.factor(region)), color="gray60",
               outlier.alpha = 0, outlier.size = NA, coef=0, alpha=0.7) +
  scale_fill_viridis_d("Region", option = "F")+
  theme_classic(base_family = "Roboto Condensed") +
  facet_wrap(as.factor(region)~.)

# calc cumulative DSCI by year
dsci_sum <- dsci %>%
  group_by(fips, wyear) %>%
  mutate(dsci_wy_sum = cumsum(dsci),
         dsci_wy_max = max(dsci_wy_sum)) %>%
  ungroup()

# plot by county
ggplot() + geom_line(data=dsci_sum, aes(x=date, y=dsci_wy_sum, color=dsci_wy_max), show.legend=FALSE) +
  scale_color_viridis_c("DSCI", option = 1)+
  theme_classic(base_family = "Roboto Condensed") +
  labs(y="Cumulative DSCI (Max Annual)", x="", caption="DSCI 2000-2023")+
  facet_wrap(~county_nm)


# plot by max per year
dsci_sum %>%
  select(wyear, fips, county_nm, region, dsci_wy_max) %>%
  distinct(.keep_all = TRUE) %>% #View()
  ggplot() +
  geom_point(aes(x=wyear, y=dsci_wy_max, color=dsci_wy_max), show.legend=FALSE) +
  geom_smooth(aes(x=wyear, y=dsci_wy_max, group=county_nm), show.legend=FALSE, color="gray20", alpha=0.2, linewidth=0.5, se = FALSE, fill=alpha("gray80", 0.1)) +
  colorspace::scale_color_continuous_diverging(palette = "Blue-Red 2", mid = 8000)+
  #scale_color_viridis_c("DSCI", option = "H")+
  theme_classic(base_family = "Roboto Condensed") +
  labs(y="Cumulative DSCI (Max Annual)", x="", caption="DSCI 2000-2023") +
  theme_classic() + facet_wrap(~county_nm)


# calculate quantiles
dsci_ann_quant <- dsci_sum %>%
  select(wyear, fips, county_nm, region, dsci_wy_max) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup() %>%
  mutate(lbound05 = quantile(dsci_wy_max, 0.05),
         ubound95 = quantile(dsci_wy_max, 0.95))


# which vals fall outside of this?
outliers <- dsci_ann_quant[which(dsci_ann_quant$dsci_wy_max > dsci_ann_quant$ubound95),]
ggplot() +
  geom_point(data=dsci_ann_quant, aes(x=wyear, y=dsci_wy_max), color="gray70",alpha=0.4, show.legend=FALSE) +
  geom_point(data=outliers, aes(x=wyear, y=dsci_wy_max, fill=as.factor(wyear)), pch=21, size=3, show.legend=TRUE) +
  #colorspace::scale_fill_continuous_diverging(palette = "Blue-Red 2", mid = 8000)+
  ggthemes::scale_fill_colorblind("WY")+
  theme_classic(base_family = "Roboto Condensed") +
  theme(legend.position = "bottom")+
  labs(y="Cumulative DSCI (Max Annual)", x="",
       title="Top 5% Extreme Drought Years by County based on Cumulative DSCI") +
  facet_wrap(~county_nm)


# calc mean DSCI by month and then sum
dsci_mon_sum <- dsci %>%
  group_by(county_nm, wyear, mon, fips) %>%
  summarize(dsci_mon_avg = mean(dsci),
         dsci_mon_sum = cumsum(dsci),
         dsci_mon_max = max(dsci_mon_sum)) %>%
  ungroup() %>%
  # add quantiles %>%
  mutate(lbound05 = quantile(dsci_mon_max, 0.05),
         ubound95 = quantile(dsci_mon_max, 0.95),
         ymon = glue("{wyear}-{mon}"))

# plot by max by month
dsci_mon_sum %>%
  ggplot() +
  geom_point(aes(x=ymon, y=dsci_mon_max, color=dsci_mon_max), show.legend=FALSE, size=1, alpha=0.7) +
  geom_smooth(aes(x=ymon, y=dsci_mon_max, group=county_nm), show.legend=FALSE, color="gray20", alpha=0.2, linewidth=0.5, se = FALSE, fill=alpha("gray80", 0.1)) +
  colorspace::scale_color_continuous_diverging(palette = "Blue-Red 2", mid = 1000)+
  #scale_color_viridis_c("DSCI", option = "H")+
  theme_classic(base_family = "Roboto Condensed") +
  labs(y="Cumulative DSCI (Max Monthly)", x="", caption="DSCI 2000-2023") +
  theme_classic() + facet_wrap(~county_nm)



# break by region and plot!
# aggregate each measure for a given region and then plot
# look at anomaly from mean?

# calc the anomaly
