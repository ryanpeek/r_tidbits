# upset plots to look at intersections (instead of venn diagrams)

library(UpSetR)
library(ggplot2)
library(grid)
library(plyr)

# data
movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"),
                   header = T, sep = ";")

# basic intersect plot with attribute histogram
upset(movies, main.bar.color = "black",
      queries = list(list(query = intersects,
                          params = list("Drama"), active = T)),
      attribute.plots = list(gridrows = 50, plots = list(list(plot = histogram, x = "ReleaseDate", queries = F),
                                                         list(plot = histogram,
                                                              x = "AvgRating", queries = T)), ncols = 2))

# intersection boxplots
upset(movies, boxplot.summary = c("ReleaseDate"))
