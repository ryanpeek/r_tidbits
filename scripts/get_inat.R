# inat

library(rinat)
library(tigris)
options(tigris_use_cache = TRUE)
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


# Save Data ---------------------------------------------------------------

# save out
# save data
save(lica_sf, lica_ca, file = glue("data_raw/bullfrog_{Sys.Date()}.rda"))
load("data_raw/bullfrog_2023-01-31.rda")

# Add counties ------------------------------------------------------------

lica_ca <- st_join(lica_ca, ca_cntys[,c(1:6)])

table(lica_ca$NAME, useNA = "ifany")

mapview(lica_ca, zcol="NAME") + mapview(ca_cntys, col.regions="gray", alpha.regions=0.1)


# get table of records by County

lica_by_cnty <- lica_ca %>% #st_drop_geometry() %>%
  filter(!is.na(NAME)) %>%
  group_by(NAME) %>% tally(sort = TRUE, name = "n_per_cnty")

# Plot Bullfrogs by County ------------------------------------------------

ggplot() + geom_pointrange(data=lica_by_cnty,
                           aes(x=forcats::fct_reorder(NAME, n_per_cnty),y=n_per_cnty, ymin=0, ymax=n_per_cnty), fill="green4", color="gray80", pch=21, alpha=0.5,linewidth=0.2, size=0.7) +
  coord_flip()+
  hrbrthemes::theme_ft_rc(base_family = "Roboto Condensed", base_size = 12)+
  labs(x="County", y="",
       subtitle = "American bullfrogs observed per County",
       caption = glue("iNaturalist {{rinat}}: pulled {Sys.Date()}"))


# ggsave(glue("figs/bullfrogs_per_cnty_iNat_{Sys.Date()}.png"), height = 11, width = 8.5, dpi=300)

# add percent urbanized or waterways or something along those lines?
# elevation gradient by range or centroid of county?
# how can we identify where to target bullfrogs?
# would be good to add data on how many ER or WA have bullfrogs detected from this dataset


# Hotspot ------------------------------------------------------------

library(patchwork)
library(ggtext)
library(camcorder)
library(tidycensus) # data(state_laea)
library(rgeoboundaries) #remotes::install_gitlab("dickoa/rgeoboundaries"

# us <- rgeoboundaries::gb_adm1("usa") %>%
#   st_crop(xmin = -125, xmax = -67, ymin = 23, ymax = 52)
#us <- tidycensus::state_laea
ca <- USAboundaries::us_states(states = "CA") #%>%
  # st_transform(3310) if using change cell size to 4k or more

# add tally by cnty
lica_ca <- lica_ca %>% group_by(NAME) %>% add_tally(name="n_per_cnty")# %>%
  #st_transform(3310)

hotspots <- sfhotspot::hotspot_gistar(lica_ca, cell_size = 0.2, grid_type = "hex", kde = FALSE)

hotspots_w <- sfhotspot::hotspot_gistar(lica_ca, cell_size = 0.2, grid_type = "hex", weights = n_per_cnty, kde = FALSE)

f1 <- "Outfit"
f2 <- "Roboto Slab"
library(showtext)
font_add_google(f1)
font_add_google(f2)
showtext_auto()

ggplot(hotspots %>% filter(gistar > 0))+ # pvalue < 0.05)) +
  geom_sf(data = ca, fill = "#E5E6E8", color = "#868D94") +
  geom_sf(data=ca_cntys, col="gray", fill=alpha("gray10", 0))+
  geom_sf(data=lica_ca, col="forestgreen", alpha=0.1, size=0.7, pch=1)+
  geom_sf(data=hotspots %>% filter(gistar > 0,  pvalue < 0.05), aes(colour = gistar, fill = gistar), alpha=0.9) +
  geom_sf(data=hotspots %>% filter(gistar > 0,  pvalue > 0.05),
          aes( fill = gistar), alpha=0.2, color=alpha("gray40", 0.5)) +
  scale_fill_stepsn(colors = MetBrewer::met.brewer("Tam"), limits = c(0, 30), breaks = seq(0, 30, 5)) +
  scale_color_stepsn(colors = MetBrewer::met.brewer("Tam"), limits = c(0, 30), breaks = seq(0, 30, 5)) +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") +
  labs(title = "American Bullfrog hotspots",
       caption = glue("Data: iNaturalist {{rinat}}, updated {Sys.Date()}")) +
  theme_void(base_family = f1, base_size = 20) +
  #hrbrthemes::theme_modern_rc(base_family = f1) +
  theme(
    #plot.background = element_rect(fill = "#868D94", color = NA),
    plot.title = element_markdown(size = 34),
    #axis.text = element_text(size = 2),
    legend.key.height = unit(2, "lines"),
    legend.key.width = unit(0.8, "lines"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, color = "gray40", family = f2)
  )


ggsave(filename = "figs/bullfrog_hotspots_via_iNat.png",
       width = 8.5, dpi = 300, bg = "white")


hotspot_plot <- function(df, title) {
  ggplot(df %>% filter(gistar > 0, pvalue < 0.05)) +
    geom_sf(data = us, fill = "#E5E6E8", color = "#868D94") +
    geom_sf(aes(colour = gistar, fill = gistar)) +
    scale_fill_stepsn(colors = MetBrewer::met.brewer("Tam"), limits = c(0, 30), breaks = seq(0, 30, 5)) +
    scale_color_stepsn(colors = MetBrewer::met.brewer("Tam"), limits = c(0, 30), breaks = seq(0, 30, 5)) +
    coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") +
    labs(title = title) +
    theme_void(base_family = f1) +
    theme(
      plot.background = element_rect(fill = "#868D94", color = NA),
      plot.title = element_markdown(size = 18, color = "grey85"),
      legend.key.height = unit(2, "lines"),
      legend.key.width = unit(0.8, "lines"),
      legend.title = element_blank(),
      legend.text = element_text(size = 13, color = "white", family = f2)
    )
}

img <- ggplot() +
  ggimage::geom_image(aes(x = 1, y = 10, image = "https://github.com/gkaramanis/tidytuesday/raw/master/2023/2023-week_02/img/grackle.png"), size = 0.5) +
  coord_fixed() +
  theme_void()

w <- hotspot_plot(hotspots_w, "<span style='color:white'>Winter</span> (Sep - Feb)")
s <- hotspot_plot(hotspots_s, "<span style='color:white'>Summer</span> (Mar - Aug)")

w / s +
  inset_element(img, 0.9, 0.95, 1.2, 1.95) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Observations of common grackle",
    subtitle = "Gi* value for cells with p-value < 0.05",
    caption = "Source: Project FeederWatch ܍ Photo: Tina Nord ܍ Graphic: Georgios Karamanis",
    theme = theme(
      plot.margin = margin(10, 40, 10, 40),
      plot.background = element_rect(fill = "#868D94", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold", family = f1, color = "white", size = 24, margin = margin(10, 0, 7, 0)),
      plot.subtitle = element_text(hjust = 0.5, family = f1, color = "white", size = 16, margin = margin(0, 0, 20, 0)),
      plot.caption = element_text(color = "#E5E6E8", family = f1)
    )
  )

