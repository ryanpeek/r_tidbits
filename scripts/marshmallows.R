# marshmallows
# assumes 90million lbs per year, scaled by us pop
library(tidyverse)

# 1. Download US population data from FRED
# POPTHM = Civilian noninstitutional population? No:
# POPTHM is the total population, in thousands, monthly.
url <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv",
  "?id=POPTHM"
)

population <- read_csv(url, show_col_types = FALSE) |>
  rename(date = observation_date, population_thousands = POPTHM) |>
  mutate(
    date = as.Date(date),
    year = as.integer(format(date, "%Y")),
    population_thousands = as.numeric(population_thousands)
  ) |>
  filter(!is.na(population_thousands))

# 2. Calculate average annual population
annual_population <- population |>
  group_by(year) |>
  summarise(
    population_thousands = mean(population_thousands),
    .groups = "drop"
  )

# 3. Set the marshmallow consumption benchmark
baseline_year <- 2006
baseline_lbs <- 90e6

baseline_population <- annual_population |>
  filter(year == baseline_year) |>
  pull(population_thousands)

if (length(baseline_population) != 1) {
  stop("Could not find the 2006 population benchmark.")
}

# 4. Estimate pounds consumed per year
marshmallow_estimates <- annual_population |>
  filter(year >= 1950, year <= 2026) |>
  mutate(
    estimated_marshmallow_lbs =
      baseline_lbs * population_thousands / baseline_population,
    estimated_marshmallow_millions_lbs =
      estimated_marshmallow_lbs / 1e6,
    estimate_type =
      "Population-scaled estimate; not observed sales"
  ) |>
  select(
    year,
    population_thousands,
    estimated_marshmallow_lbs,
    estimated_marshmallow_millions_lbs,
    estimate_type
  )

# 5. Export CSV
write_csv(
  marshmallow_estimates,
  "data_raw/us_marshmallow_estimates_1950_2026.csv"
)

# 6. Plot annual estimates
ggplot(
  marshmallow_estimates,
  aes(x = year, y = estimated_marshmallow_millions_lbs)
) +
  geom_line(color = "#E89B50", linewidth = 1.1) +
  labs(
    title = "Estimated U.S. Marshmallow Consumption, 1950–2026",
    subtitle = paste(
      "Population-scaled scenario anchored at",
      "90 million pounds in 2006"
    ),
    x = "Year",
    y = "Estimated consumption (million pounds)",
    caption = paste(
      "Modeled estimates, not measured sales.",
      "Assumes constant per-capita consumption."
    )
  ) +
  theme_minimal(base_size = 12)
