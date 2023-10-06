# merge csv

# load library
library(readxl)
library(tidyverse)
library(janitor)
library(fs)
library(glue)

# filename
csv_dir <- "~/Downloads/pre_post/"

ls_files <- fs::dir_ls(csv_dir, glob = "*csv")

# a function:
get_data <- function(csv_file, survey_type, class, skip_rows, seq_rows){

  # read in
  df_dat <- read_csv(csv_file, lazy = TRUE)

  # label and tidy
  df_out <- df_dat |> slice(skip_rows:nrow(df_dat)) |>
    # get only stats rows and columns
    slice(seq(1, nrow(df_dat), seq_rows)) |>
    dplyr::select(2, 5:8) |>
    # rename columns
    rename_all(~c("Question", "Mean", "Stdev", "Variance", "Count")) |>
    rownames_to_column(var="Q_id") |>
    mutate(survey_type = survey_type,
           class = janitor::make_clean_names(class)) |>
    # format vals
    mutate(across(c(Q_id, Mean, Stdev, Variance, Count), ~as.numeric(.x)))

  df_out <- df_out |> filter(!is.na(Mean))

  print("Done!")
  return(df_out)
}

# test
class <- "ENL 149 Sp22"
skip_rows <- 6 # mostly 6
seq_rows <- 11 # mostly 11, but some biosci is 14
surv_type <- "pre"
dpre <- get_data(glue("{csv_dir}{class} {surv_type}.csv"), surv_type, class, skip_rows, seq_rows)

surv_type <- "post"
dpst <- get_data(glue("{csv_dir}{class} {surv_type}.csv"), surv_type, class, skip_rows, seq_rows)

# join
dat_out <- bind_rows(dpre, dpst) |>
  mutate(survey_type = factor(survey_type, levels=c("pre", "post")))

# save out
write_csv(dat_out, file = glue("{csv_dir}{make_clean_names(class)}_comparison.csv"))

# quick plot
dat_out |> group_by(survey_type, class) |>
  ggplot() + geom_col(aes(x=Q_id, y=Mean, fill=survey_type), position = position_dodge()) +
  ggthemes::scale_fill_colorblind("Survey")+
  coord_flip() +
  scale_x_continuous(labels=c(1:22), breaks = c(1:22)) +
  cowplot::theme_half_open() +
  labs(subtitle = glue("{class}"))

ggsave(filename = glue("{csv_dir}{make_clean_names(class)}_barplot.png"), width = 10, height = 8, dpi=300, bg="white")
