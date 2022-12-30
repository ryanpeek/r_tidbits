# get precip data from noaa

library(httr)
library(glue)
library(fs)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(tigris)
library(ggplot2)
library(ggdark)


# Path --------------------------------------------------------------------

# https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series/CA-017/pcp/36/10/1895-2022.csv?filter=true&filterType=loess

# list of counties
cntys <- list_counties("CA")

county_sel <- c("Kern", "Sacramento", "El Dorado", "Sonoma")
cnty <- filter(cntys, county %in% county_sel) %>%
  pull(county_code)

# years
yr_start <- 2000 # can start 1895
yr_end <- 2022

# month
mon <- 08

# interval: 1-12 mon, 18,24, 36, 48, 60
interval <- 24

# build url for county CA
make_url <- glue("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series/CA-{cnty}/pcp/{interval}/{mon}/{yr_start}-{yr_end}.csv?base_prd=true&begbaseyear={yr_start}&endbaseyear={yr_end}&filter=true&filterType=binomial")

# use url
dat_out <- read_csv(make_url, skip = 4, id = "path")
dat_out <- dat_out %>%
  mutate(county_code = substr(gsub("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series/", "", path), 4,6)) %>%
  select(-path) %>%
  left_join(cntys)
# convert to dates
dat_out <- dat_out %>%
  mutate(date = ymd(paste(Date,"01")))

# plot
ggplot() + geom_line(data=dat_out, aes(x=date, y=Anomaly, color=county)) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed") +
  scale_x_date(date_breaks = "years", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=60, hjust=1))+
  scale_color_brewer("County", type = "qual", palette = "Dark2")+
  labs(title = glue("Precip Anomaly for {month.abb[{mon}]}: Aggegated over {interval} months"),
       x="", y="Precip Anomaly",
       caption="Data source: Precipitation from <www.ncei.noaa.gov/access/monitoring/climate-at-a-glance>")

