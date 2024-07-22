library(tidyverse)

# have col a and col b, want col c. Col c says "for a given value of col a, if all the values in col b are yes, make col c yes; if any are no's make col c no." The number of rows per value in col a can vary, and there can be NAs and a couple other values in col B that I'd like to make rules around


df <- data.frame(
  a = c(1, 1, 2, 2, 3, 3, 3, 4, 4),
  b = c("Y", "Y", "Y", "N", NA, "N", "InPrgs", "Y", NA),
  c = c("Y", "Y", "N", "N", "N", "N", "N", "Y", "Y")
)

# replace all NA's as char for search ability
df[is.na(df)]<-"NA"

# try case_when?
df <- df |>
  group_by(a) |>
  mutate(colC = case_when(
    grepl("^N$", b) ~ "N",
    grepl("^Y$", b) ~ "Y"
  ))


