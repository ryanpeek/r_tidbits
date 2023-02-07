# read sturgeon data

library(janitor)
library(tidyverse)
library(fs)
library(lubridate)
library(glue)
library(here)
library(purrr)

# some nice tips on big data here: https://inbo.github.io/tutorials/tutorials/r_large_data_files_handling/

# Data --------------------------------------------------------------------

db_dir <- "~/Downloads/sturgeon_db/"
# this is all XLSX files (includes CCAM, YRBD, and YRAD OLD).
(csv_paths <- fs::dir_ls(db_dir, 
                         recurse = TRUE, # look through all folders
                         # files ending in csv
                         regexp = "(?i)green_sturgeon(.*).csv.zip$"))
length(csv_paths) # n=12

# get just basename and drop file extension .csv.zip
gsub(path_file(csv_paths), pattern = ".csv.zip", replacement = "")

# Read in Data ------------------------------------------------------------

# function to read in and format
get_zipped_csv_data <- function(path_to_file){
  read_csv(path_to_file,
           id = "filepath", 
           show_col_types = TRUE) %>% 
    # drop last 2 rows, or try w "n": if (n > 0) {head(df_csv, -n))}
    filter(row_number() <= n()-2) %>% # or use slice(1:(n()-2))
    # add column that is just filename
    mutate(filename = gsub(path_file(filepath), pattern = ".csv.zip", replacement = ""), .after=filepath)
}

# some files are diff classes!!

# chr  (13): Species, FishID, StudyID, PI, Length, Length_Type, Codespace, Sex, Life_Stage, DetectionLo...
# dbl   (5): TagID, Lat, Lon, RiverKm, VR2SN
# dttm  (3): DateTagged, Date_Released, DetectDate

# chr (19): Species, FishID, TagID, StudyID, PI, Length, Length_Type, Codespace, DateTagged, Date_Relea...
# dbl  (2): RiverKm, VR2SN

# chr  (12): Species, FishID, StudyID, PI, Length_Type, Codespace, Sex, Life_Stage, DetectionLocation, ...
# dbl   (6): TagID, Length, Lat, Lon, RiverKm, VR2SN
# dttm  (3): DateTagged, Date_Released, DetectDate

# test with single
# df_csv <- get_zipped_csv_data(csv_paths[11])


# Apply to All ------------------------------------------------------------

# will be BIG
dfs <- map(csv_paths, ~get_zipped_csv_data(.x))

# how many lines in full dataset?
map(dfs, ~sprintf("Number of lines in full data set: %s", nrow(.x)))

# dfs_df <- map_df(csv_paths, ~get_zipped_csv_data(.x)) 
# of course...this fails bc lat and lon are diff (num and char)
map(dfs, ~str(.x$Lat))
map(dfs, ~str(.x$Lon))

dfs2 <- modify_at(dfs, .at = c(16, 17), ~as.character)
map(dfs2, ~select(.x$Lon, .x$Lat) %>% head)

# when saved how big? (53MB)
# save(dfs, file = "data_output/sturgeon_example.rda")
