# H3 example
library(duckdbfs)
library(dplyr)
library(units)

duckdb_s3_config(s3_endpoint  = "s3-west.nrp-nautilus.io",s3_url_style = "path", anonymous = TRUE)

area <- open_dataset("s3://public-ca30x30/conserved-areas-terrestrial-2025/hex-weights") |>
  summarise(km2 = sum((w1 + w2) * h3_cell_area(h10, "km^2"), na.rm = TRUE)) |>
  pull(km2)

set_units(area, km^2) |> set_units(acres)


# compare area in CA with https://discreteglobal.wpengine.com/ ?
# ISEA3H Hexagons

# or this one
# https://explorer.natureserve.org/api-docs/#_the_nested_hexagon_framework
