# premier league crosses

# see here: https://github.com/sonofacorner/soc-viz-of-the-week/tree/main/01022023

library(tidyverse)
library(glue)
library(vroom)

# Download Orig Data --------------------------------------------------------------------

#crosses_dat <- read_csv("https://github.com/sonofacorner/soc-viz-of-the-week/blob/main/01022023/data/2022_2023_premier_league.csv?raw=true")

#vroom::vroom_write(crosses_dat, "data_raw/premiere_league_crosses.tsv.gz")


# Read in Data ------------------------------------------------------------

dat <- vroom::vroom("data_raw/premiere_league_crosses.tsv.gz")
