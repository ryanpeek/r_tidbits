## snippets

# fit a basic multiple linear regression model
library(equatiomatic)
m <- lm(bill_length_mm ~ bill_depth_mm + flipper_length_mm, penguins)
extract_eq(m)
