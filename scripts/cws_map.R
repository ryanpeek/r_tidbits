# cws map
# 2026

# libraries:
library(pacman)
pacman::p_load(tidyverse,
               hrbrthemes,
               janitor,
               glue,
               patchwork,
               nhdplusTools,
               sf,
               ggspatial,
               rmapshaper,
               tigris,
               showtext,
               colorspace,
               ggrepel,
               lwgeom)
options(tigris_use_cache = TRUE)


# Fonts and Themes --------------------------------------------------------

# Custom fonts:
font_ucd <- "Ryman-Regular"
#sysfonts::font_files() %>% View()
font_title <- 'Source Code Pro'
sysfonts::font_add_google(font_title)
font_title2 <- "Public Sans"
font_add_google(font_title2)

showtext_opts(dpi = 300, regular.wt = 200, bold.wt = 700)
showtext::showtext_auto(enable = TRUE)
text_size <- 13
river_col <- "#4783A9"
ucd_blue <- "#004aa8"
ucd_gold <- "#d9b000"
ucd_gold2 <- "#FFBF00"
# UCD Blue: 100/56/0/34 #004aa8
# UCD Gold: 0/19/100/15

# Get State & County ------------------------------------------------------

# get state cnty boundaries
ca <- tigris::states() |> filter(STUSPS=="CA")
cnty <- tigris::counties(state="CA")


# Get Delta and Lake Tahoe ------------------------------------------------

#pak::pkg_install("InteragencyEcologicalProgram/deltamapr")
library(deltamapr)
#plot(deltamapr::WW_Delta$geometry)
#plot(deltamapr::WW_Watershed$geometry)

# Get HUCS ----------------------------------------------------------------

# get flowlines based on AOI
ca_hucs <- nhdplusTools::get_huc(ca, type = "huc08")

# check if all same?
all((st_geometry_type(ca_hucs)=="POLYGON")==TRUE)
# make all same
ca_hucs <- st_make_valid(ca_hucs) |> st_cast("POLYGON")
all((st_geometry_type(ca_hucs)=="POLYGON")==TRUE)

# split
# any duplicate?
ca_hucs %>% st_drop_geometry() %>% group_by(huc8) %>% tally() %>% filter(n>1)
# drop dups (largely islands off CA main)
ca_hucs_distinct <- ca_hucs %>% distinct(huc8, .keep_all = TRUE)

# simplify:
ca_hucs_distinct_s <- rmapshaper::ms_simplify(ca_hucs_distinct, keep = 0.1)

# make a list of hucs we can use
ca_hucs_ls <- split(ca_hucs_distinct_s, ca_hucs_distinct$huc8)
#map(ca_hucs_ls, ~dim(.x))

# select some hucs for testing
huc_sel <- ca_hucs_distinct %>% filter(grepl("North Fork American", name))

# get lake tahoe water: H16050101, Reachcode: 16050101000339, Permanent ID: 44560536, GNIS_ID: 01654975, COMID outlet: 120053784
tahoe <- get_waterbodies(id = 120053784)
#plot(tahoe$geometry)

# TEST -----------------------------------------------------------------

## Get NHD from API --------------------------------------------------------

# or grab via nhdtools
# nhd_api <- nhdplusTools::get_nhdplus(AOI = ca_hucs |> sample_n(1), realization = "flowline", streamorder = 1)
#
# # single watershed
# nhd_out <- nhdplusTools::get_nhdplus(AOI = huc_sel %>% filter(grepl("North Fork American", name)), realization = "flowline", streamorder = 1)


## Clip to Boundary --------------------------------------------------------

# # then clip to watershed
# nhd_flow <- st_intersection(nhd_out, huc_sel)
#
# # add a col for plotting stream level/order
# nhd_flow <- nhd_flow %>%
#   mutate(streamorder_log = log(streamorde+1))
#
# table(nhd_flow$streamorder_log)


## Simplify Lines ----------------------------------------------------------

# simplify lines:
# nhd_flow_s <- rmapshaper::ms_simplify(nhd_flow,weighting = 0.99)
# weighting 0 is basically straight lines

## Plot --------------------------------------------------------------------

## Plot Base ---------------------------------------------------------------

# plot
# plot(huc_sel$geometry, lwd=1.4, border=alpha("gray", 0.8))
# plot(nhd_flow_s$geometry, lwd=nhd_flow_s$streamorder_log,
#      col=alpha(river_col, 0.7),add=TRUE)


## ggplot: streamorder -----------------------------------------------------

# make plot by streamorder
# (gg1 <- ggplot() +
#    # can add huc
#    #geom_sf(data=huc_sel, fill = png_bkg, color = NA)+
#    # add flowlines with order level as factor
#    geom_sf(data=nhd_flow_s,
#             aes(linewidth = factor(case_when(
#               streamorde >= 5 ~ "major",
#               streamorde == 4 ~ "large",
#               streamorde == 3 ~ "medium",
#               TRUE ~ "small"))
#             ),
#             color = river_col) +
#    # now add scale for linewidth
#    scale_linewidth_manual(
#      values = c(major = 0.5, large = 0.35,
#                 medium = 0.25, small = 0.1),
#      guide = "none") +
#    # add dark theme without axis or grid
#    hrbrthemes::theme_ft_rc(grid = FALSE, axis = FALSE) +
#    labs(title="Rivers",
#         caption = "R. Peek • Data: NHDPlus") +
#    theme(plot.title =
#            element_text(family=font_title2, hjust=0.5),
#          plot.subtitle =
#            element_text(family=font_annot2, color = alpha("orange",0.7), size=40, hjust=0.5),
#          plot.caption = element_text(family=font_annot, color="gray90", hjust=0.5),
#          axis.text.x = element_blank(),
#          axis.text.y = element_blank()))
#

## ggplot: with inset ------------------------------------------------------

# inset
# (ca_inset <- ggplot() +
#    geom_sf(data = ca) +
#    geom_sf(data = huc_sel,  fill = NA,
#            color = "blue", linewidth = 0.4) +
#    theme_void() +
#    theme(
#      geom = element_geom(
#        linewidth = 0.15,
#        color = "grey40",
#        fill = "white")))
#
# # set plot margin
# plot_margin <- 0.05
# (c1 <- cowplot::ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
#   cowplot::draw_plot(gg1,
#                      x = 0 + plot_margin,
#                      y = 0 - 0.8 * plot_margin,
#                      width = 1 - 2 * plot_margin) +
#   # inset map for orientation
#   cowplot::draw_plot(ca_inset,
#                      x = .58,
#                      y = 0.65,
#                      width = 0.25,
#                      height = 0.25,
#                      hjust = 0, vjust = 0) +
#   cowplot::draw_label("CWS\nUC Davis",
#                       fontfamily = font_annot,
#                       x = 0.3,
#                       y = 0.3,
#                       size = text_size,
#                       color = "orange")
# )
#
# cowplot::save_plot("figs/2026_cws_rivs_peek.png", plot = c1, base_width =  10, dpi=300, bg="transparent")
#

## ggplot: with path/line label ------------------------------------------------------
#
# # make spot for label
# crds <- tibble(X=c(-121.025, -121.2), Y=c(38.94, 38.95)) %>%
#   mutate(group = 1)
#
# # now add in
# gg1 +
#   geomtextpath::geom_textpath(
#     data = crds,
#     aes(x = X, y = Y, label = "CWS", group = group),
#     family = font_annot,
#     size = 3,
#     text_smoothing = 20,
#     text_only = FALSE, # remove the path, show text only
#     vjust  = -0.9,
#     hjust  = 1.63,
#     arrow = grid::arrow(angle = 20, ends = "first"),
#     color = "gray90")



# Get ALL NHD from API --------------------------------------------------------

# fetch all: this takes AWHILE
#ca_flines <- map(ca_hucs_ls, safely(~nhdplusTools::get_nhdplus(AOI = .x, realization = "flowline", streamorder = 2)))

# save all out
# save(ca_flines, file = "data_out/ca_flines.rda")

load("data_out/ca_flines.rda")

# fetch some
#ca_flines_1 <- map(ca_hucs_ls[1:5], safely(~nhdplusTools::get_nhdplus(AOI = .x, realization = "flowline", streamorder = 2)))

# drop unused levels and drop errors
ca_flines_trim <- compact(ca_flines)
ca_flines_trim <- map(ca_flines_trim, c(1))

# get outlets comid first
outlet_comids <- map_int(ca_flines_trim, ~.x$comid[which.max(.x$totdasqkm)])

# bind together
ca_flines_df <- do.call("rbind", ca_flines_trim)

# get end points of outlets
library(lwgeom)
outlet_pts <- st_endpoint(ca_flines_df %>% filter(comid %in% outlet_comids))

# create dataframe for CWS
cws <- tibble(y=38.5377, x=-121.7579, label="CWS") %>%
  st_as_sf(coords=c("x","y"), crs=4269, remove=FALSE)

# use ggrepel to create arrow label

# Final Plot --------------------------------------------------------------
river_c1 <- "#CAE1FF"
river_c2 <- "#00C4B3" # arboretum
river_c3 <- "#00B2E3" # tahoe
river_c4 <- "#0047BA" # gunrock
river_c5 <- "#76236C" # pinot
pt_c1 <- "#03F9E6" # rain

# make plot by streamorder
(gg2 <- ggplot() +
    # CA boundary
    geom_sf(data=ca, lwd=1, alpha=0.8, color=river_c4, fill=NA)+

    # add flowlines with order level as factor
    geom_sf(data=ca_flines_df,
            aes(linewidth = factor(case_when(
              streamorde >= 5 ~ "major",
              streamorde == 4 ~ "large",
              streamorde == 3 ~ "medium",
              TRUE ~ "small"))),
            color = ucd_gold) +

    # now add scale for linewidth
    scale_linewidth_manual(
      values = c(major = 0.5, large = 0.35,
                 medium = 0.25, small = 0.1),
      guide = "none") +
    # add tahoe
    geom_sf(data=tahoe, fill=ucd_blue, color=ucd_blue, linewidth=0.1) +
    # add delta
    geom_sf(data=WW_Watershed, fill=ucd_blue, color=ucd_blue, linewidth=0.2) +

    # OUTLETS
    geom_sf(data=outlet_pts, pch=21, fill=river_c1, size=2.5) +


    # add CWS
    ggrepel::geom_text_repel(data=cws,
                             aes(x=x, y=y, label=label),
                             nudge_x = -5,
                             min.segment.length = 1,
                             size=5.2,
                             inherit.aes = FALSE,
                             box.padding = 5,
                             nudge_y = -2,
                             family=font_title2, fontface="bold",
                             color=river_c1, segment.color=river_c1,
                             segment.curvature = .3,
                             segment.angle = 30
    ) +
    geom_sf(data=cws, pch=21, fill=pt_c1, size=5) +

    # add dark theme without axis or grid
    hrbrthemes::theme_ft_rc(grid = FALSE, axis = FALSE) +
    labs(caption = "R. Peek • NHD Outlets") +
    theme(plot.title =
            element_text(family=font_title2, size = 20, hjust=0.5),
          plot.caption = element_text(family=font_title2, color="gray90", hjust=0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()))

# add image
library(ggimage)

gg3 <- gg2 + geom_image(
  data = tibble(x = -116.5, y = 41.5),
  aes(x=x, y=y, image = "~/Downloads/CWS_logo_2.png"),
  size = 0.3
)
gg3

# save out
ggsave(gg3, filename="figs/2026_cws_rivs_peek_ca_outlets_Yellow.pdf",
       width = 8.5, height = 11,
       dpi=600, bg="transparent")



# White Rivers Background --------------------------------------------------------

# make plot by streamorder
(gg4 <- ggplot() +
   # CA boundary
   geom_sf(data=ca, lwd=1, alpha=0.8, color=river_c4, fill=NA)+

   # add flowlines with order level as factor
   geom_sf(data=ca_flines_df,
           aes(linewidth = factor(case_when(
             streamorde >= 5 ~ "major",
             streamorde == 4 ~ "large",
             streamorde == 3 ~ "medium",
             TRUE ~ "small"))),
           color = river_c1) +

   # now add scale for linewidth
   scale_linewidth_manual(
     values = c(major = 0.5, large = 0.35,
                medium = 0.25, small = 0.1),
     guide = "none") +

      # add tahoe
   geom_sf(data=tahoe, fill=river_c4, color=river_c4, linewidth=0.1) +
   # add delta
   geom_sf(data=WW_Watershed, fill=river_c4, color=river_c4, linewidth=0.2) +
   # add points of outlets
   geom_sf(data=outlet_pts, pch=21, fill=pt_c1, size=2.5) +


   # add CWS
   ggrepel::geom_text_repel(data=cws,
                            aes(x=x, y=y, label=label),
                            nudge_x = -5, size=5.2,
                            min.segment.length = 1,
                            inherit.aes = FALSE,
                            box.padding = 5,
                            nudge_y = -2,
                            family=font_title2, fontface="bold",
                            color=ucd_gold2, segment.color=ucd_gold2,
                            segment.curvature = .3,
                            segment.angle = 30
   ) +
   geom_sf(data=cws, pch=21, fill=ucd_gold2, size=5) +

   # add dark theme without axis or grid
   hrbrthemes::theme_ft_rc(grid = FALSE, axis = FALSE) +
   labs(caption = "R. Peek • NHD Outlets") +
   theme(plot.title =
           element_text(family=font_title2, size = 20, hjust=0.5),
         plot.caption = element_text(family=font_title2, color="gray90", hjust=0.5),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank()))

# add image
library(ggimage)

gg5 <- gg4 + geom_image(
  data = tibble(x = -116.5, y = 41.5),
  aes(x=x, y=y, image = "~/Downloads/CWS_logo_2.png"),
  size = 0.3
)
gg5

# save out
ggsave(gg5, filename="figs/2026_cws_rivs_peek_ca_outlets_White.pdf",
       width = 8.5, height = 11,
       dpi=600, bg="transparent")




## ggplot: with path/line label ------------------------------------------------------

# # make spot for label
# crds <- tibble(X=c(-121.025, -121.2), Y=c(38.94, 38.95)) %>%
#   mutate(group = 1)
#
# # now add in
# gg1 +
#   geomtextpath::geom_textpath(
#     data = crds,
#     aes(x = X, y = Y, label = "CWS", group = group),
#     family = font_annot,
#     size = 3,
#     text_smoothing = 20,
#     text_only = FALSE, # remove the path, show text only
#     vjust  = -0.9,
#     hjust  = 1.63,
#     arrow = grid::arrow(angle = 20, ends = "first"),
#     color = "gray90")
#
#
#
# # Test Plot ---------------------------------------------------------------
#
#
#
# # plot
# library(ggnewscale)
#
# # geom_sf(data=rivers_swe, aes(color=as.factor(ORD_STRA), size=as.factor(ORD_STRA)), show.legend = FALSE)+
# #   scale_color_manual(name = "",
# #                      values = rev(c('#08306b', '#08519c', '#2171b5', '#4292c6', '#6baed6', alpha('#6baed6',0.7), alpha('#6baed6',0.4)))) +
# #   scale_size_manual(values=c("1" = 0.1, "2" = 0.2, "3" = .45, "4" = .7, "5" = 1.2, "6"=2, "7"=3.1)) +
#
# gg_1 <- ggplot() +
#   geom_sf(data=ca, fill=NA, color="gray50", linewidth=1, alpha=0.4) +
#   #geom_sf(data= ca_hucs, fill=NA, color="steelblue", alpha=0.4) +
#   #coord_sf(label_graticule = "", datum = NA) +
#   geom_sf(data=h8_flines_df, aes(color=as.factor(streamorde), size=as.factor(streamorde)), show.legend = FALSE)+
#   scale_color_manual(name = "",
#                      values = rev(c('forestgreen', 'green3', 'green2', 'green', alpha('chartreuse',0.7),
#                                     alpha('chartreuse',0.7), alpha('chartreuse',0.4)))) +
#   scale_size_manual(values=c("3" = 0.1, "4" = 0.2, "5" = .45, "6" = .7, "7" = 1.2, "8"=2, "9"=3.1)) +
#   geom_sf(data=h8_term_pts, color=alpha("white",0.6), pch=16, size=4) +
#   geom_sf(data=h8_term_pts, fill="yellow", color=alpha("white",0.9), pch=21, size=2.5) +
#   hrbrthemes::theme_ft_rc(grid = FALSE, axis = FALSE) +
#   labs(title="Terminal Points to Rivers",
#        subtitle = "#30DayMapChallenge • Day-26 • Minimal",
#        caption = "R. Peek • Data: NHDPlus HUC8 Data, Stream Order >=4") +
#   theme(plot.title =
#           element_text(family ="Hepta Slab", size = 60, color = "green2", hjust=0.5),
#         plot.subtitle =
#           element_text(family="Hepta Slab", color = alpha("green2",0.7), size=40, hjust=0.5),
#         plot.caption = element_text(family="Hepta Slab", color="gray90",size=24, hjust=0.5),
#         legend.title =
#           element_text(family = "Hepta Slab", face="bold", size=34),
#         legend.text =
#           element_text(family = "Hepta Slab", face="bold", size=34),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
#
# gg_1
