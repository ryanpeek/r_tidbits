# make a barplot

# libraries --------
library(readr) # for importing data
library(dplyr) # for wrangling data
library(forcats) # for dealing with categorical data/factors
library(ggplot2) # for plotting
library(cowplot) # great themes for plotting and saving plots

# import data ---------

spp <- read_csv("data_raw/Full_Sp_Scores.csv")

# PLOT ---------------------

## basic plot ----------

# if a column has spaces or non standard characters, we need to wrap it in backticks (``).
table(spp$`SSC Rank`) # see how many exist for each

# plot
ggplot() + geom_col(data=spp, aes(x=Score, y=Taxon , fill=`SSC Rank`))

## Plot Ordered by Score ----------

# the forcats package is great for this
ggplot() + geom_col(data=spp, aes(x=Score, y=forcats::fct_reorder(Taxon, Score), fill=`SSC Rank`))

## Plot clean up labels -------------

# with 166 different spp, I would recommend not plotting them all together as it will overwhelm the plot and make nothing legible, even with tiny fonts

ggplot() + geom_col(data=spp, aes(x=Score, y=forcats::fct_reorder(Taxon, Score), fill=`SSC Rank`)) +
  labs(x="SSC Score", y="") + # fix x and y
  theme_half_open(font_size = 6) +
  cowplot::background_grid(major="x", )


# Filter to Top 50% of scores or only SSC Priority -------------

# try filtering to only data in top 50% (can turn ties on/off)
spp_filt <- spp %>%
  slice_max(order_by = Score, prop = 0.50, with_ties = TRUE)

# now replot
ggplot() + geom_col(data=spp_filt, aes(x=Score, y=forcats::fct_reorder(Taxon, Score), fill=`SSC Rank`)) +
  labs(x="SSC Score", y="") + # fix x and y
  theme_half_open(font_size = 6) +
  cowplot::background_grid(major="x", )

# Try Facets ----------------------

# let's try faceting so only the SSC or Listed spp are in one panel and the no status/unlisted are in another
ggplot() + geom_col(data=spp_filt, aes(x=Score, y=forcats::fct_reorder(Taxon, Score), fill=`SSC Rank`)) +
  labs(x="SSC Score", y="") + # fix x and y
  theme_half_open(font_size = 6) +
  cowplot::background_grid(major="x", )+
  facet_wrap(. ~ `SSC or Listed?`)

# Try just the data that is listed or SSC. Here we can "pipe" data into the ggplot using the %>%

spp %>%
  filter(`SSC or Listed?`=="Yes") %>% # passing or piping data to next step
  # notice we take the data= argument out here because it's piped in
  ggplot() + geom_col(aes(x=Score, y=forcats::fct_reorder(Taxon, Score), fill=`SSC Rank`), color=alpha("gray30", 0.5)) +
  labs(x="SSC Score", y="", title = "Species of Special Concern Ranking") + # fix x and y
  theme_half_open(font_size = 9) +
  cowplot::background_grid(major="x")

# save a plot! Default to last plot plotted, or specify one saved
ggsave(filename = "ssc_barplot_by_score_only_listed.png", width = 8, height = 11, units = "in", dpi = 300, bg = "white")

