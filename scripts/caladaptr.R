# read in shp

library(terra)
library(sf)
library(tidyverse)
library(pacman)
library(units)
# get shp:
shp <- st_read("~/Downloads/casc_frog_range_wgs84/casc_frog_range_wgs84.shp")

# CalAdaptR ---------------------------------------------------------------
# URL: https://ucanr-igis.github.io/caladaptr
#remotes::install_github("ucanr-igis/caladaptr")
library(caladaptr)

# simplify shp:
shp_simple <- rmapshaper::ms_simplify(shp, keep = 0.5)
shp_simple <- st_make_valid(shp_simple)

# cal adapt data
# most historical is 1950-2013
# most future is 2006-2100

raca_cap <- ca_loc_sf(loc=shp_simple,idfld = "OBJECTID") %>%
  ## select GCM(s)
  ca_gcm(c("CNRM-CM5", "MIROC5", "CCSM4", "CanESM2", "HadGEM2-CC", "ens32avg")) %>%
  ca_scenario(c("rcp45","rcp85")) %>%                               ## select emission scenarios(s)
  ca_cvar(c("swe")) %>%                                          ## select climate variables
  ca_period("month") %>%                                             ## select a temporal aggregation period
  ca_years(start = 2020, end = 2050)

raca_cap

# plot first:
plot(raca_cap, locagrid = TRUE, static = TRUE)

# can fetch as stars, tbl, or db:
# ca_getvals_tbl(), ca_getvals_db() ca_getrst_stars()
raca_tbl <- raca_cap %>%
  ca_getvals_tbl(quiet = TRUE)

head(raca_tbl)

# historical
raca_hist <- ca_loc_sf(loc=shp_simple,idfld = "OBJECTID") %>%
  ## select GCM(s)
  ca_gcm(c("CNRM-CM5", "MIROC5", "CCSM4", "CanESM2", "HadGEM2-CC", "ens32avg")) %>%
  ca_scenario(c("historical")) %>%                               ## select emission scenarios(s)
  ca_cvar(c("swe")) %>%                                          ## select climate variables
  ca_period("month") %>%                                             ## select a temporal aggregation period
  ca_years(start = 1950, end = 2000)

raca_hist

raca_hist_tbl <- raca_hist %>%
  ca_getvals_tbl(quiet = TRUE)



# Plot and Summarize ------------------------------------------------------

# plot

# historical
raca_hist_tbl %>%
  mutate(mon = month(as.Date(dt))) %>%
  filter(mon %in% c(3)) %>%
  group_by(gcm, scenario, mon) %>%
  summarize(mean_swe = mean(val)) %>%
  ggplot() + geom_boxplot(aes(x = as.factor(mon), y = mean_swe, color=gcm, group=gcm)) +
  #facet_grid(gcm ~ .) +
  labs(title = "Historical (1950-2000) Snow Water Equivalent (SWE)", subtitle="Over Cascades Frog Range", x = "month", y = "SWE")


# future
raca_tbl %>%
  mutate(mon = month(as.Date(dt))) %>%
  filter(mon %in% c(3)) %>%
  group_by(gcm, scenario, mon) %>%
  summarize(mean_swe = mean(val)) %>%
  ggplot() + geom_boxplot(aes(x = as.factor(mon), y = mean_swe, color=gcm, group=gcm)) +
  facet_grid(scenario ~ .) +
  labs(title = "Future (2020-2050) Snow Water Equivalent (SWE)", subtitle="Over Cascades Frog Range", x = "month", y = "SWE")

# do both
raca_hist_swe <- raca_hist_tbl %>%
  mutate(mon = month(as.Date(dt))) %>%
  filter(mon %in% c(3)) %>%
  group_by(gcm, scenario, mon) %>%
  summarize(mean_swe = mean(val))

raca_futu_swe <- raca_tbl %>%
  mutate(mon = month(as.Date(dt))) %>%
  filter(mon %in% c(3)) %>%
  group_by(gcm, scenario, mon) %>%
  summarize(mean_swe = mean(val))

raca_all <- bind_rows(raca_hist_swe, raca_futu_swe) %>%
  mutate(mean_swe_in = set_units(mean_swe, "in"))

ggplot(data=raca_all) + geom_boxplot(aes(x = as.factor(mon), y = mean_swe_in, color=scenario, group=scenario)) +
  #facet_grid(scenario ~ .) +
  labs(title = "Predicted Change in Snow Water Equivalent (SWE) \nacross Cascades Frog Range", subtitle="CalAdapt Data, Historical: 1950-2000, Future: 2020-2050", x = "Apr 1 Mean", y = "SWE")

ggsave("figs/raca_swe_shift.png", width = 6, height = 4, dpi=300, bg="white")

write_csv(raca_all, "data_out/caladpatr_raca_swe_hist_future.csv")
