# Look at Drought And FLow Periods


# Libraries ---------------------------------------------------------------

library(fs)
library(dataRetrieval)
library(readr)
library(glue)

## Function ------------

# make a function to download DAILY data

# function: dates as "YYYY-MM-DD"
get_daily_flow <- function(gage_no){

  # create folder to save data
  fs::dir_create("data_raw")

  # set parameters to download data
  siteNo <- gage_no # The USGS gage number
  pCode <- "00060" # 00060 is discharge parameter code

  # get NWIS daily data: CURRENT YEAR
  dat <- readNWISdv(siteNumbers = siteNo,
                    parameterCd = pCode)
  # add water year
  dat <- addWaterYear(dat)
  # rename the columns
  dat <- renameNWISColumns(dat)

  # save out
  write_csv(dat,
            file =
              glue("data_raw/nfa_updated_{Sys.Date()}.csv"))
}

## Run function -------------

siteNo <- "11427000" # NF American River
get_daily_flow(siteNo)



# Clean and Viz -----------------------------------------------------------

library(tidyverse)
library(lubridate)

# get file info to identify most recent
files_info <- fs::dir_info("data_raw",
                           regexp = "nfa_updated")

# get most recent file only if multiple exists
(file_recent <- files_info %>%
    select(path, modification_time) %>%
    arrange(modification_time) %>%
    slice_max(order_by = modification_time))

# remove any other files
files_info %>% filter(path!=file_recent$path) %>%
  pull(path) %>%
  fs::file_delete()

# read in most recent
df <- read_csv(glue("{file_recent$path}"))

# clean AND RENAME
df_clean <- df %>%
  rename(date = Date, water_year=waterYear) %>%
  mutate(year = year(date), month = month(date),
         #water_year = year(date) + ifelse(month(date) >= 10, 1, 0),
         water_day = (date - as.Date(sprintf('%s-10-01', water_year)))) %>%
  group_by(water_year) %>%
  mutate(water_day = as.numeric(water_day - min(water_day) + 1),
         Flow_cms = Flow * 0.0283168) %>%
  ungroup() |>
  select(site_no, date, month, water_year, water_day, Flow, Flow_cms, Flow_cd, agency_cd)


# Water Stats -------------------------------------------------------------

library(FlowScreen)

# select data of interest and make into time series
df_ts <- df_clean %>%
  select(site_no, date, Flow, Flow_cd, agency_cd) %>%
  as.data.frame() %>%
  # convert to cms
  mutate(Flow = Flow * 0.0283168,
         PARAM = 1) %>%
  # rename
  rename(Date=date, ID=site_no, SYM=Flow_cd, Agency=agency_cd) %>%
  select(ID, PARAM, Date, Flow, SYM, Agency) %>%
  create.ts()

# use pk.cov function to calc center of volume
df_cov <- pk.cov(df_ts)

# same as this:
tst <- df_clean %>%
  group_by(water_year) %>%
  summarize(totvol=cumsum(Flow_cms)/sum(Flow_cms)) %>%
  bind_cols(., df_clean[,c(1:2, 5, 6:7)])


# VISUALIZE ---------------------------------------------------------------

## cumulative flow volume by year-------------

plotly::ggplotly(

  ggplot() + geom_line(data=tst, aes(x=water_day, y=totvol,group=water_year, color=water_year)) +
    ggdark::dark_theme_classic(base_family = "Roboto Condensed") +
    theme(axis.text.x = element_text(angle=60, hjust=1))+
    scale_color_viridis_c() +
    labs(x="Day of water year", y="Total Volume")
)


## plot all with current year and prev 2 yrs----------------
(p1<-ggplot(data=df_ts) +
   geom_line(aes(x=hdoy, y=Flow, group=hyear), color="gray", alpha=0.8, lwd=0.3) +
   geom_line(data=df_ts %>% filter(hyear>=year(Sys.Date())+1),
             aes(x=hdoy, y=Flow), color="steelblue", linewidth=2) +
   # add drought years
   geom_line(data=df_ts %>% filter(hyear==2017),
             aes(x=hdoy, y=Flow), color="darkblue", lwd=0.5) +
   geom_line(data=df_ts %>% filter(hyear==2020),
             aes(x=hdoy, y=Flow), color="red4", lwd=0.5) +
   geom_line(data=df_ts %>% filter(hyear==2021),
             aes(x=hdoy, y=Flow), color="maroon", lwd=0.5) +
   geom_line(data=df_ts %>% filter(hyear==2022),
             aes(x=hdoy, y=Flow), color="brown3", lwd=0.5) +
   geom_point(data=df_ts %>% filter(hyear>=year(Sys.Date())+1) %>%
                slice_max(hdoy, n = 1),
              aes(x=hdoy, y=Flow), pch=21, size=4, fill="orange") +
   theme_classic() +
   labs(title=glue("Flow from USGS: {unique(df_clean$site_no)},
                  {min(df_clean$water_year)}-{max(df_clean$water_year)}"),
        caption=glue("Updated {Sys.Date()}"),
        x="Day of Water Year", y="Flow (cms)"))


## drought severity --------------------------------------

df_drought <- FlowScreen::dr.seas(df_ts, WinSize = 30, Season = 1:12)
df_drought$hyear <- year(attr(df_drought$StartDay, "times"))

# get quantiles
#(thresh <- df_ts %>% filter(hyear==2017) %>% group_by(hyear) %>% summarize(quants = quantile(Flow, 0.2, na.rm=TRUE)))

# drought metrics
ggplot(data=df_drought) +
  geom_linerange(aes(xmax=EndDay, xmin=StartDay, y=hyear, color=Severity), lwd=2) +
  geom_point(aes(x=StartDay, y=hyear), pch=21, size=2, fill="red2", alpha=0.7) +
  geom_point(aes(x=EndDay, y=hyear), pch=21, size=3, fill="maroon4") +
  scale_color_viridis_c("Severity", option = "A") +
  scale_y_continuous(breaks = c(seq(1941, 2021, 4))) +
  theme_classic() +
  labs(title="Start and end of significant droughts", y="Water Year",
       x="Day of Water Year",
       subtitle="Severity of drought based on 30-day window using a flow duration curve (Beyene et al. 2014)")

# save
#ggsave(filename = "output/figure_drought_periods.png",
#       dpi=200, width=10, height = 6.5)



## center of volume plot ----------------------------------------

(plot_cov <-
   ggplot() +
   geom_point(data=df_cov, aes(y=hYear, x=Q50, fill=Dur),
              pch=21, size=3,
              color="gray20",show.legend=TRUE) +
   # add drought years
   geom_point(data=df_cov %>%
                filter(hYear %in% c(2014:2016, 2020:2022)),
              aes(y=hYear, x=Q50, fill=Dur),
              pch=21, size=3,
              color="white",show.legend=TRUE) +

   geom_vline(xintercept=150, color="steelblue", lty=2) +
   scale_fill_viridis_c("Duration", option = "B") +
   #scale_color_viridis_d("Water Year\n Type") +
   scale_y_continuous(breaks = seq(1942, year(Sys.Date()),4)) +
   hrbrthemes::theme_ft_rc() +
   labs(title = "NFA Center of Volume (50% of total annual streamflow)",
        y="Water Year", x="Day of year",
        subtitle="Duration in days between the 25 percent and 75 percent total annual streamflow")
)


