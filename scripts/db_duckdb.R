# using duck db

library(duckdb)
library(tidyverse)
library(stringr)


# DUCKDB: Stocks ----------------------------------------------------------

# list of URLs containing our csv files
base_url <- "https://raw.githubusercontent.com/Robot-Wealth/r-quant-recipes/master/data/"
stocks <- c("AAPL", "BA", "CAT", "IBM", "MSFT")
urls <- str_glue("{base_url}{stocks}.csv")

# read data into a dataframe
prices_df <- urls %>%
  map_dfr(read_csv, col_types = "Dddddddc") %>%
  arrange(Date, Ticker)

head(prices_df)

# make an in-memory db and store the connection in a variable
con <- dbConnect(duckdb::duckdb())

# to make a persistent db on disk (and to connect to it again later) do:
# con <- dbConnect(duckdb::duckdb(dbdir = "/path/to/db/location/mydatabase.duckdb"))

# write our prices data to duckdb table
table_name <- "prices"
duckdb::dbWriteTable(con, table_name, prices_df)

# remove prices_df
rm(prices_df)

tbl(con, "prices") %>%
  group_by(Ticker) %>%
  summarise(
    Count = n(),
    From = first(Date),
    To = last(Date)
  )
(result <- dbGetQuery(con, paste0("SELECT * FROM prices LIMIT 5")))

tbl(con, "prices") %>%
  head(5) %>%
  collect()

# make query more SQL friendly
# will work
tbl(con, "prices") %>%
  dplyr::mutate(TWAP = (Open + High + Low + Close)/4) %>%
  head()


# 60-day moving average
affected_rows <- dbExecute(con,
                           'ALTER TABLE prices ADD COLUMN IF NOT EXISTS MA60 DOUBLE;
     UPDATE prices SET MA60 = t2.MA60
     FROM (
        SELECT Ticker, Date, AVG("Adj Close") OVER (
            PARTITION BY Ticker ORDER BY Date ROWS BETWEEN 59 PRECEDING AND CURRENT ROW
        ) AS MA60
    FROM prices
    ) t2
WHERE prices.Ticker = t2.Ticker AND prices.Date = t2.Date;'
)

affected_rows

# check cols
tbl(con, "prices") %>% head()


# DUCKDB: FLIGHTS ---------------------------------------------------------

library(duckdb)
library(dplyr)
con <- dbConnect(duckdb())
duckdb_register(con, "flights", nycflights13::flights)

tbl(con, "flights") |>
  group_by(dest) |>
  summarise(delay = mean(dep_time, na.rm = TRUE)) |>
  collect()

# Establish a set of Parquet files
dbExecute(con, "COPY flights TO 'dataset' (FORMAT PARQUET, PARTITION_BY (year, month))")

# Summarize the dataset in DuckDB to avoid reading 12 Parquet files into R's memory
tbl(con, "read_parquet('dataset/**/*.parquet', hive_partitioning = true)") |>
  filter(month == "3") |>
  summarise(delay = mean(dep_time, na.rm = TRUE)) |>
  collect()


# DUCKDBFS ------------------------------------

# https://github.com/cboettig/duckdbfs
devtools::install_github("cboettig/duckdbfs")
library(duckdbfs)
library(dplyr)

base <- paste0("https://github.com/duckdb/duckdb/raw/main/",
               "data/parquet-testing/hive-partitioning/union_by_name/")
f1 <- paste0(base, "x=1/f1.parquet")
f2 <- paste0(base, "x=1/f2.parquet")
f3 <- paste0(base, "x=2/f2.parquet")
urls <- c(f1,f2,f3)

ds <- open_dataset(urls, unify_schemas = TRUE)
ds

## DUCKDBFS: S3 ---------------------

parquet <- "s3://gbif-open-data-us-east-1/occurrence/2023-06-01/occurrence.parquet"
duckdb_s3_config()
gbif <- open_dataset(parquet, anonymous = TRUE, s3_region="us-east-1")

head(gbif)

# anonymous access to a bucket with alt endpoint
efi <- open_dataset("s3://anonymous@neon4cast-scores/parquet/aquatics?endpoint_override=data.ecoforecast.org")

head(efi)

## DUCKDBFS: Spatial ---------------------

spatial_ex <- paste0("https://raw.githubusercontent.com/cboettig/duckdbfs/",
                     "main/inst/extdata/spatial-test.csv") |>
  open_dataset(format = "csv")

spatial_ex |>
  mutate(geometry = st_point(longitude, latitude)) |>
  mutate(dist = st_distance(geometry, st_point(0,0))) |>
  to_sf(crs = 4326)

## vectors
url <- "https://github.com/cboettig/duckdbfs/raw/main/inst/extdata/world.fgb"
countries <- open_dataset(url,
                          format = "sf")

countries |> head()

# get metadata
countries_meta <- st_read_meta(url)
countries_meta

# bring it all into R mem
in_mem <- countries |> to_sf(crs = countries_meta$wkt)

# work outside of in mem
library(sf)
melbourne <- st_point(c(144.9633, -37.814)) |> st_as_text()

countries |>
  filter(st_contains(geom, ST_GeomFromText({melbourne})))

sf_obj <- countries |> filter(continent == "Africa") |> to_sf()
plot(sf_obj["name"])


## DUCKDBFS: SPATIAL JOINS ----------------

url_cities <- "https://github.com/cboettig/duckdbfs/raw/spatial-read/inst/extdata/metro.fgb"
cities <- open_dataset(url_cities, format="sf")

# check meta to see crs matches
countries_meta$proj4
st_read_meta(url_cities)$proj4

# return all points within collection of polygons
countries |>
  dplyr::filter(continent == "Oceania") |>
  spatial_join(cities, by = "st_intersects", join="inner") |>
  select(name_long, sovereignt, pop2020)


## DUCKDBFS: WRITE -----------

mtcars |> group_by(cyl, gear) |> write_dataset(tempfile())

# READ FROM CSV:
# write.csv(mtcars, "mtcars.csv", row.names=FALSE)
# lazy_cars <- open_dataset("mtcars.csv", format = "csv")

# duck spatial
# https://github.com/duckdb/duckdb_spatial
#https://francoismichonneau.net/2023/06/duckdb-r-remote-data/
