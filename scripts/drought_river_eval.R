# Look at Drought And FLow Periods


# Libraries ---------------------------------------------------------------

library(fs)
library(dataRetrieval)
library(readr)
library(glue)
library(geomtextpath)
library(wateRshedTools)
library(tidyverse)
library(lubridate)
library(FlowScreen)

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

# join w WYT
df_cov <- left_join(df_cov, wateRshedTools::ca_wytypes[,c(1,6,11)], by=c("hYear"="WY")) %>%
  # manually add 2022 for now:
  mutate(
    sv_WYtype = case_when(
      hYear == 2022 ~ "C",
      TRUE ~ sv_WYtype),
    sj_WYtype = case_when(
      hYear == 2022 ~ "C",
      TRUE ~ sj_WYtype),
    sj_WYtype = forcats::fct_relevel(sj_WYtype, c("C","D","BN","AN","W")),
    sv_WYtype = forcats::fct_relevel(sv_WYtype, c("C","D","BN","AN","W")))
levels(ca_wytypes$sj_WYtype)
levels(ca_wytypes$sv_WYtype)


df_clean <- left_join(df_clean, wateRshedTools::ca_wytypes[,c(1,6,11)], by=c("water_year"="WY")) %>%
  # manually add 2022 for now:
  mutate(
    sv_WYtype = case_when(
      water_year == 2022 ~ "C",
      TRUE ~ sv_WYtype),
    sj_WYtype = case_when(
      water_year == 2022 ~ "C",
      TRUE ~ sj_WYtype),
    sj_WYtype = forcats::fct_relevel(sj_WYtype, c("C","D","BN","AN","W")),
    sv_WYtype = forcats::fct_relevel(sv_WYtype, c("C","D","BN","AN","W")))

# same as this:
tst <- df_clean %>%
  group_by(water_year) %>%
  summarize(totvol=cumsum(Flow_cms)/sum(Flow_cms)) %>%
  bind_cols(., df_clean[,c(1:2, 5, 6:7, 10:11)])

# check levels
table(df_clean$sv_WYtype, useNA = "ifany")
table(df_clean$sj_WYtype, useNA = "ifany")

# get drought severity
df_drought <- FlowScreen::dr.seas(df_ts, WinSize = 30, Season = 1:12)
df_drought$hyear <- year(attr(df_drought$StartDay, "times"))

# join w wytype
df_drought <- left_join(df_drought, wateRshedTools::ca_wytypes[,c(1,6,11)], by=c("hyear"="WY")) %>%
  # manually add 2022 for now:
  mutate(
    sv_WYtype = case_when(
      hyear == 2022 ~ "C",
      TRUE ~ sv_WYtype),
    sj_WYtype = case_when(
      hyear == 2022 ~ "C",
      TRUE ~ sj_WYtype),
    sj_WYtype = forcats::fct_relevel(sj_WYtype, c("C","D","BN","AN","W")),
    sv_WYtype = forcats::fct_relevel(sv_WYtype, c("C","D","BN","AN","W")))

# get quantiles
#(thresh <- df_ts %>% filter(hyear==2017) %>% group_by(hyear) %>% summarize(quants = quantile(Flow, 0.2, na.rm=TRUE)))

# VISUALIZE ---------------------------------------------------------------

## Plot: cumulative flow volume by year -----------------------

#plotly::ggplotly(

  ggplot() +
    geom_line(data=tst, aes(x=water_day, y=totvol,group=water_year, color=sv_WYtype)) +
    geom_textpath(data=tst |> filter(water_year == 2013), aes(x=water_day, y=totvol, label=water_year), color="maroon", vjust=-0.2, hjust=0.5)+
    geom_textpath(data=tst |> filter(water_year == 2015), aes(x=water_day, y=totvol, label=water_year), color="maroon", vjust=-0.2, hjust=0.6)+
  geom_textpath(data=tst |> filter(water_year == 2014), aes(x=water_day, y=totvol, label=water_year), color="red4", vjust=-0.2, hjust=0.64, text_smoothing = 20)+
  geom_textpath(data=tst |> filter(water_year == 1991), aes(x=water_day, y=totvol, label=water_year), color="red4", vjust=1.3, hjust=0.4, text_smoothing = 20)+
  geom_textpath(data=tst |> filter(water_year == 2022), aes(x=water_day, y=totvol, label=water_year), color="maroon2", vjust=-0.2, hjust=0.45, text_smoothing = 30)+
    geom_textpath(data=tst |> filter(water_year == 1997), aes(x=water_day, y=totvol, label=water_year), color="cyan3", vjust=-0.2, hjust=0.5)+
    ggdark::dark_theme_classic(base_family = "Roboto Condensed") +
    theme(axis.text.x = element_text(angle=60, hjust=1))+
    scale_color_viridis_d("WY Type") +
    labs(x="Day of water year", y="Total Volume",
         subtitle = "NFA: Cumulative Flow Volume Oct 1-Sep 30") +
  facet_wrap(~sv_WYtype)
#)

ggsave("figs/cumulative_flow_vol_nfa.png", width = 11, height = 8, dpi=300)

# would be cool to add centroid "point" for each of these "types" and think about what makes a drought year extreme

## Plot: flow w current year----------------

(p1<-ggplot(data=df_ts) +
   geom_line(aes(x=hdoy, y=Flow, group=hyear), color="gray", alpha=0.8, lwd=0.3) +
   geom_line(data=df_ts %>% filter(hyear>=year(Sys.Date())),
             aes(x=hdoy, y=Flow), color="steelblue", linewidth=1.4) +
   # add drought years
   geom_line(data=df_ts %>% filter(hyear==2017),
             aes(x=hdoy, y=Flow), color="darkblue", linewidth=0.5) +
   geom_line(data=df_ts %>% filter(hyear==2020),
             aes(x=hdoy, y=Flow), color="red4", linewidth=0.5) +
   geom_line(data=df_ts %>% filter(hyear==2021),
             aes(x=hdoy, y=Flow), color="maroon", linewidth=0.5) +
   geom_line(data=df_ts %>% filter(hyear==2022),
             aes(x=hdoy, y=Flow), color="brown3", linewidth=0.5) +
   geom_point(data=df_ts %>% filter(hyear>=year(Sys.Date())) %>%
                slice_max(hdoy, n = 1),
              aes(x=hdoy, y=Flow), pch=21, size=4, fill="orange") +
   theme_classic() +
   labs(title=glue("Flow from USGS: {unique(df_clean$site_no)},
                  {min(df_clean$water_year)}-{max(df_clean$water_year)}"),
        caption=glue("Updated {Sys.Date()}"),
        x="Day of Water Year", y="Flow (cms)"))


## Plot: drought severity --------------------------------------


# drought metrics
ggplot(data=df_drought) +
  geom_linerange(aes(xmax=EndDay, xmin=StartDay, y=hyear, color=Severity), lwd=2) +
  geom_point(aes(x=StartDay, y=hyear, fill=sv_WYtype), pch=21, size=2, alpha=0.7) +
  geom_point(aes(x=EndDay, y=hyear, fill=sv_WYtype), pch=21, size=3) +
  scale_color_viridis_c("Severity", option = "A") +
  ggthemes::scale_fill_colorblind("WYT") +
  scale_y_continuous(breaks = c(seq(1942, 2022, 4))) +
  theme_classic(base_family = "Roboto Condensed") +
  cowplot::background_grid(major = "y")+
  labs(title="Start and end of significant droughts", y="Water Year",
       x="Day of Water Year",
       subtitle="Severity of drought based on 30-day window using a flow duration curve (Beyene et al. 2014)")

# save
ggsave(filename = "figs/figure_drought_periods.png",
       dpi=200, width=10, height = 6.5)


## Plot: center of volume ----------------------------------------

(plot_cov <-
   ggplot() +
   geom_point(data=df_cov, aes(y=hYear, x=Q50, fill=sv_WYtype),
              pch=21, size=3.5,
              color="gray40",show.legend=TRUE) +
   # add drought years
   # geom_point(data=df_cov %>%
   #              filter(hYear %in% c(2014:2016, 2020:2022)),
   #            aes(y=hYear, x=Q50, fill=sv_WYtype),
   #            pch=21, size=3,
   #            color="white",show.legend=TRUE) +

   geom_vline(xintercept=150, color="steelblue", lty=2) +
   scale_fill_viridis_d("WYT", option = "B") +
   #scale_color_viridis_d("Water Year\n Type") +
   scale_y_continuous(breaks = seq(1942, year(Sys.Date()),4)) +
   hrbrthemes::theme_ft_rc() +
   ggrepel::geom_text_repel(data=df_cov, aes(y=hYear, x=Q50, label=hYear), color="gray70", size=2, family="Roboto Slab") +
   labs(title = "NFA Center of Volume (50% of total annual streamflow)",
        y="Water Year", x="Day of year")
        #subtitle="Duration in days between the 25 percent and 75 percent total annual streamflow")
)

plotly::ggplotly(plot_cov)

# facet
plot_cov + facet_grid(~sv_WYtype)

# Plot: high flows ------------------------------------------------------

n17 <- readNWISuv(siteNo, "00060", startDate = "2016-10-01", endDate = "2017-09-29") %>%
  renameNWISColumns() %>%
  addWaterYear() %>%
  mutate(dowy=wateRshedTools::dowy(dateTime))

nnow <- readNWISuv(siteNo, "00060", startDate = "2022-10-01") %>%
  renameNWISColumns() %>%
  addWaterYear() %>%
  mutate(dowy=wateRshedTools::dowy(dateTime))

# set up labels
lab1 <- n17 %>% slice_max(order_by = Flow_Inst, n=1) %>%
  mutate(label = " Flows over **20,000 cfs**<br>have occurred 20 times since 1965.<br>This was the 9th highest flow on record")

ggplot() + geom_line(data=n17, aes(x=dateTime, y=Flow_Inst, color=Flow_Inst), show.legend = FALSE)+
  geom_point(data=n17 %>% slice_max(order_by = Flow_Inst, n=1), aes(x=dateTime, y=Flow_Inst), pch=21, fill="skyblue", size=3)+
  ggtext::geom_richtext(data=lab1, aes(x=dateTime, y=Flow_Inst, label=label),
                        hjust = -0.1, vjust=1, size=2.5, fill = NA, label.color = NA) +
  geom_line(data=nnow, aes(x=dateTime, y=Flow_Inst, color=Flow_Inst), show.legend = FALSE)+
  geom_point(data=nnow %>% slice_tail(n=1), aes(x=dateTime, y=Flow_Inst), pch=21, fill="skyblue", size=3)+
  scale_color_gradientn("cfs", colors=MetBrewer::met.brewer("Isfahan1")) +
  facet_wrap(.~waterYear, scales = "free_x") +
  hrbrthemes::theme_ipsum_ps() +
  labs(x="", y="Flow (cfs)", subtitle = "NF American River (water years)", caption="(@riverpeek) Data from USGS gage 1142700")

ggsave(filename = "figs/nfa_2017_vs_current.png", width = 11, height = 8, dpi=300, bg = "white")

# find max
n17 %>%
  group_by(dowy) %>%
  summarize(maxflow = max(Flow_Inst)) %>%
  slice_max(order_by = maxflow, n=3)

nnow %>%
  group_by(dowy) %>%
  summarize(maxflow = max(Flow_Inst)) %>%
  slice_max(order_by = maxflow, n=3)
