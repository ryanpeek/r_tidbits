# create duckdb

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(duckdb)
library(duckplyr)
library(palmerpenguins)

# create an empty database ---------------------------------

con <- dbConnect(duckdb(), dbdir = "data_raw/example.duckdb", read_only = FALSE)

# load data
glimpse(penguins_raw)

# make a couple tables
df <- penguins_raw |> janitor::clean_names() |>
  select(study_name:date_egg)

df_raw <- penguins_raw |> janitor::clean_names()


# Write Tables ------------------------------------------------------------

dbWriteTable(con, "df", df, overwrite = TRUE)
dbWriteTable(con, "df_raw", df_raw, overwrite = TRUE)

dbDisconnect(con)
rm(con)

# Reconnect ---------------------------------------------------------------

con <- dbConnect(duckdb(), dbdir = "data_raw/example.duckdb", read_only = TRUE)
dbListTables(con)
df <- tbl(con, 'df')
df_raw <- tbl(con, 'df_raw')

# more query
df |> select(region, island, date_egg) |>
  head(10)

df |> select(region, island, date_egg) |>
  head(10) |> collect()



# Query Larger Tables -----------------------------------------------------

rs <- dbSendQuery(con, "SELECT * FROM df LIMIT 10")
DBI::dbColumnInfo(rs)

res <- dbGetQuery(con, "SELECT * FROM iris_table LIMIT 1")
print(res)



# Trying without loading data first into R --------------------------------
# https://r4ds.hadley.nz/databases
# duckdb_read_csv() and duckdb_register_arrow()

# https://r4ds.hadley.nz/iteration#reading-multiple-files
# load a bunch of csv's into a db
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_read_csv(con, "gapminder", paths)

## con <- dbConnect(duckdb(), dbdir = "my-db.duckdb", read_only = TRUE)
## close: dbDisconnect(con, shutdown = TRUE)


# one approach using duckdb and dplyr
library("duckdb")
library("dplyr")
con <- dbConnect(duckdb())
duckdb_register(con, "flights", nycflights13::flights)

tbl(con, "flights") |>
  group_by(dest) |>
  summarise(delay = mean(dep_time, na.rm = TRUE)) |>
  collect()
