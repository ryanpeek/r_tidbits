
# sankey plot
# example from here: https://www.johnmackintosh.net/blog/2022-01-25-energy-network/


# Libraries ---------------------------------------------------------------

library(data.table)
library(dplyr)
library(tidyr)
library(networkD3)
library(htmlwidgets)

# Data --------------------------------------------------------------------

data <- data.table::fread('https://raw.githubusercontent.com/johnmackintosh/energy-closures/main/rawdata.csv')
head(data,10)


# Wrangle -----------------------------------------------------------------

# plot by year and change columns
data2 <- data %>%
  rename(failed = `Failed_supplier`,
         acquired =  `Acquired_by`) %>%
  mutate(year_acquired = lubridate::year(Date)) %>%
  relocate(year_acquired, .before = 'Date') %>%
  select(-Date)

# reshape and format
links <-
  data2 %>%
  filter(year_acquired >= 2021) %>%
  mutate(row = row_number()) %>%
  gather('column', 'source', -row) %>%
  mutate(column = match(column, names(data2))) %>%
  group_by(row) %>%
  arrange(column) %>%
  mutate(target = lead(source)) %>%
  ungroup() %>%
  filter(!is.na(target))

# make nodes
links <-
  links %>%
  mutate(source = paste0(source, '_', column)) %>%
  mutate(target = paste0(target, '_', column + 1)) %>%
  select(source, target)

# more wrangle
nodes <- data.frame(name = unique(c(links$source, links$target)))

links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1
links$value <- 1

nodes$name <- sub('_[0-9]+$', '', nodes$name)


# Plot --------------------------------------------------------------------

sankeyNetwork(Links = links,
              Nodes = nodes,
              Source = 'source',
              Target = 'target',
              Value = 'value',
              NodeID = 'name')
