# using targets

# https://carpentries-incubator.github.io/targets-workshop/quarto.html

# https://carpentries-incubator.github.io/targets-workshop/aio.html


source("R/packages.R")
source("R/functions.R")

tar_plan(
  tar_file_read(
    penguins_data_raw,
    path_to_file("penguins_raw.csv"),
    read_csv(!!.x, show_col_types = FALSE)
  ),
  penguins_data = clean_penguin_data(penguins_data_raw)
)


library(targets)
library(tarchetypes)

source("R/functions.R")

tar_plan(
  tar_file_read(
    hello,
    "_targets/user/data/hello.txt",
    readLines(!!.x)
  ),
  hello_caps = toupper(hello),
  tar_file(
    hello_caps_out,
    write_lines_file(hello_caps, "_targets/user/results/hello_caps.txt")
  )
)
