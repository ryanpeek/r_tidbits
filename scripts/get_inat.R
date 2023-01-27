# inat

library(rinat)
library(tigris)
library(spData)
library(glue)
library(mapview)
mapviewOptions(fgb=FALSE)
library(tidyverse)
library(sf)

# Get a County ------------------------------------------------------------
ca_cntys <- tigris::counties("CA")
ca_eldor <- ca_cntys %>%
  filter(NAME=="El Dorado")

bounds <- ca_eldor


# Get Data ----------------------------------------------------------------

# search by area
lica <- get_inat_obs(query="Bullfrog", bounds = bounds, geo = TRUE, maxresults = 9999)

# plot
ggplot() + geom_point(data=lica, aes(x=longitude, y=latitude), alpha=0.5, col="green4") +
  geom_sf(data=ca_eldor, fill=NA)

# inspect
table(lica$quality_grade)

# show non ids:
lica %>% filter(scientific_name!="Lithobates catesbeianus") %>% View()

# Get all data ------------------------------------------------------------

ca <- spData::us_states[us_states$NAME=="California",]
bounds <- ca

# pull data
lica <- get_inat_obs(query="Bullfrog", bounds = bounds, geo = TRUE, maxresults = 9999)

# make spatial
lica_sf <- st_as_sf(lica, coords=c("longitude", "latitude"), crs=4326, remove=FALSE) %>%
  st_transform(4269)

# crop to CA
lica_ca <- lica_sf[ca,]

# static plot
ggplot() +
  geom_sf(data=ca_cntys, fill=NA, col="orange", alpha=0.5)+
  geom_sf(data=ca, fill=NA, col="gray30", lwd=2)+
  geom_sf(data=lica_ca, aes(fill=quality_grade), size=0.5, pch=21, alpha=0.3)+
  ggspatial::annotation_north_arrow() +
  ggthemes::theme_map()

# dynamic plot
mapview(lica_ca, zcol="quality_grade")


# save out
# save data
save(lica_sf, file = glue("data_raw/bullfrog_{Sys.Date()}.rda"))


# Add counties ------------------------------------------------------------

lica_ca <- st_join(lica_ca, ca_cntys[,c(1:6)])

table(lica_ca$NAME, useNA = "ifany")

mapview(lica_ca, zcol="NAME") + mapview(ca_cntys, col.regions="gray", alpha.regions=0.1)


# get table of records by County

lica_by_cnty <- lica_ca %>% st_drop_geometry() %>%
  filter(!is.na(NAME)) %>%
  group_by(NAME) %>% tally(sort = TRUE, name = "n_per_cnty")

ggplot() + geom_pointrange(data=lica_by_cnty,
                           aes(x=forcats::fct_reorder(NAME, n_per_cnty),y=n_per_cnty, ymin=0, ymax=n_per_cnty), fill="green4", color="gray80", pch=21, alpha=0.5,linewidth=0.2, size=0.7) +
  coord_flip()+
  hrbrthemes::theme_ft_rc(base_family = "Roboto Condensed", base_size = 12)+
  labs(x="County", y="",
       subtitle = "American bullfrogs observed per County",
       caption = glue("iNaturalist {{rinat}}: pulled {Sys.Date()}"))


ggsave(glue("figs/bullfrogs_per_cnty_iNat_{Sys.Date()}.png"), height = 11, width = 8.5, dpi=300)

# add percent urbanized or waterways or somethign along those lines?
# elevation gradient by range or centroid of county?
# how can we identify where to target bullfrogs?
# would be good to add data on how many ER or WA have bullfrogs detected from this dataset


