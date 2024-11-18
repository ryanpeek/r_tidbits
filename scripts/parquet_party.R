# working with parquet
# see great tutorial here:
# https://hutchdatascience.org/data_snacks/r_snacks/parquet.html

download.file(url = "https://github.com/aws-samples/acclerate-research-with-real-world-data-using-aws-data-exchange-for-amazon-redshift/raw/refs/heads/main/titanic.parquet", destfile = "data_raw/titanic.parquet")

# check files
library(nanoparquet)
list.files("data_raw/", pattern = "*parquet")


# Get Info ----------------------------------------------------------------

# get basic size cols/rows
nanoparquet::parquet_info("data_raw/titanic.parquet")

# get col info:
nanoparquet::parquet_column_types("data_raw/titanic.parquet")


# Setup DuckDB ------------------------------------------------------------

library(duckdb)
library(duckplyr)
con <- dbConnect(duckdb())


# Create a View -----------------------------------------------------------

# this allows us to load/look at the data
dbExecute(con,
          "CREATE VIEW titanic AS
   SELECT * FROM PARQUET_SCAN('data_raw/titanic.parquet');")

# then can
library(dplyr)
tbl(con, "titanic") |>
  head() |>
  collect()


# Do Stuff! ---------------------------------------------------------------

survived_fare <- tbl(con, "titanic") |>
  group_by(Survived) |>
  summarise(mean_fare = mean(Fare, na.rm=TRUE)) |>
  collect()

survived_fare

# explain the query:
tbl(con, "titanic") |>
  group_by(Survived) |>
  summarise(mean_fare = mean(Fare, na.rm=TRUE)) |>
  explain()
