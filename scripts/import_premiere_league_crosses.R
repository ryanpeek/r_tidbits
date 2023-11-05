# premier league crosses

# see here: https://github.com/sonofacorner/soc-viz-of-the-week/tree/main/01022023

library(tidyverse)
library(glue)
library(vroom)

# Download Orig Data --------------------------------------------------------------------

#crosses_dat <- read_csv("https://github.com/sonofacorner/soc-viz-of-the-week/blob/main/01022023/data/2022_2023_premier_league.csv?raw=true")

#vroom::vroom_write(crosses_dat, "data_raw/premiere_league_crosses.tsv.gz")

# worldfootballR
library(worldfootballR)
eng_match_results <- load_match_results(country = "ENG", gender = c("M", "F"), season_end_year = c(2020:2022), tier = "1st")
dplyr::glimpse(eng_match_results)

shts <- load_understat_league_shots(league = "EPL")

library(ggsoccer)

ggplot() +
  annotate_pitch() +
  theme_pitch()

ggplot() +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  geom_point(data=shts |>  filter(X<0.5), aes(x = X*100, y = Y*100, fill=result), alpha=0.5, pch=21, size=2) +
  #geom_point(data=shts |> filter(result=="Goal"), aes(x = X*100, y = Y*100, fill=xG), pch=21, color="yellow2", alpha=0.8) +
  scale_fill_viridis_d() +
  #coord_flip(xlim = c(49, 101)) +
  theme_pitch() +
  direction_label()


# Read in Data ------------------------------------------------------------

dat <- vroom::vroom("data_raw/premiere_league_crosses.tsv.gz")
