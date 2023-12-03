# get precip data from noaa

library(tidyverse)
library(lubridate)
library(httr)
library(glue)
library(fs)
library(tigris)
library(ggdark)


# Pull Data --------------------------------------------------------------------
# https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/divisional/mapping/4/pdsi/202207/1/value
# https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/time-series/CA-017/pcp/36/10/1895-2022.csv?filter=true&filterType=loess

# list of counties
cntys <- list_counties("CA")

county_sel <- c("Kern", "Sacramento", "El Dorado", "Sonoma")
cnty <- filter(cntys, county %in% county_sel) %>%
  pull(county_code)

# years
yr_start <- 1895 # can start 1895
yr_end <- 2022

# month
mon <- 08

# interval: 1-12 mon, 18,24, 36, 48, 60
interval <- 36

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


# Plot --------------------------------------------------------------------

ggplot() +
  geom_line(data=dat_out, aes(x=date, y=Anomaly, color=county)) +
  geom_point(data=dat_out, aes(x=date, y=Anomaly, color=county), size=1, alpha=0.3) +
  ggdark::dark_theme_classic(base_family = "Roboto Condensed") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand=c(0,365)) +
  theme(axis.text.x = element_text(angle=60, hjust=1, size=8))+
  scale_color_brewer("County", type = "qual", palette = "Dark2")+
  labs(title = glue("Precip Anomaly for {month.abb[{mon}]}: Aggegated over {interval} months"),
       x="", y="Precip Anomaly",
       caption="Data source: Precipitation from <www.ncei.noaa.gov/access/monitoring/climate-at-a-glance>")


# Bulk Download Info ------------------------------------------------------

# https://www.ncei.noaa.gov/pub/data/cirs/climdiv/
# remotes::install_github("ryanpeek/wateRshedTools")

shps <- wateRshedTools::get_shp_zip("https://www.ncei.noaa.gov/pub/data/cirs/climdiv/CONUS_CLIMATE_DIVISIONS.shp.zip")

# get just CA
climdiv_ca <- shps %>% filter(STATE=="California")
# simplify
cd_ca <- rmapshaper::ms_simplify(climdiv_ca, keep = 0.6)
pryr::object_size(cd_ca)
pryr::object_size(climdiv_ca)

# plot
#mapview::mapview(climdiv_ca, col.regions=NA, lwd=5, color="forestgreen") + mapview::mapview(cd_ca, color="orange", col.regions=NA, lwd=1, alpha.regions=0)

plot(cd_ca$geometry, border="orange", lwd=0.5)

# save out
write_rds(cd_ca, "data_out/climdiv_ca.rds")


# Now Pull Data -----------------------------------------------------------

# climate division data

curr_pcp <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpndv-v1.0.0-20231106"

dat_out <- rio::import(curr_pcp, format = "tsv",
                       colClasses=c("character", rep("numeric",12)))
# rename
colnames(dat_out) <- c("id",month.abb)

# separate (see here: https://www.ncei.noaa.gov/pub/data/cirs/climdiv/divisional-readme.txt)
dat_out <- dat_out %>%
  mutate(state_fips = substr(id, 1,2),
         climdiv = substr(id, 3,4),
         data_code = substr(id, 5, 6),
         year = as.integer(substr(id, 7,11))) %>%
  relocate(state_fips:year, .after = id)

# filter to CA climate regs
climdiv_ca$CD_2DIG

# precip only
dat_ca <- dat_out %>% filter(state_fips=="04")
table(dat_ca$climdiv)

# plot by climate div
g1 <- ggplot() + geom_sf(data=cd_ca) + geom_sf_label(data=cd_ca, aes(label=CD_2DIG))
g2 <- ggplot() + geom_line(data=dat_ca |> filter(year>2000), aes(x=year, y=Jan, color=climdiv), show.legend = FALSE) +
  facet_wrap(~climdiv)

library(patchwork)
g2 + inset_element(g1, left = 0.6, bottom = -0.1, right = 1, top = 0.3)

wrap_plots(g2, g1)
