## Rename photos sequentially
## R. Peek, 2024
## Rename photos sequentially by date including a Site ID
## this is important to do before uploading to Wildlife Insights.
## it can be done directly on the SD card or from a directory on your computer

# Libraries ---------------------------------------------------------------

library(tidyverse) # various naming/tidying functions
library(glue) # pasting names together
library(janitor) # cleaning filenames
library(fs) # file/directory metadata
library(digest) # for uniq names

# Set Paths ---------------------------------------------------------------

# CHANGE/CHECK THESE!
year <- 2024 # year of project
cam_site <- "WOMY4" # Site ID (match exactly)

# Full path to folder where photos are located
# this function helps select the folder and ensures there are images in the folder to use
select_dir <- function(){
  print("Select any image file WITHIN the folder you want to use:")
  dirname(file.choose(new = TRUE))
}

photo_directory <- select_dir()
photo_directory # double check this is correct!

# Get Photo File List -----------------------------------------------------

# get a complete list of all the photos on the card or in the directory
# note recurse=FALSE, no nested folders can be present
# we want just the directory where photos exist
photo_list <- fs::dir_info(photo_directory, type = "file", recurse = TRUE)

# filter out any videos (AVI):
photo_list <- photo_list |> filter(!fs::path_ext(path)=="AVI")

# add pheno_name:
photo_list <- photo_list |>
  mutate(
    file_name = fs::path_file(path),
    file_path = fs::path_dir(path),
    datetime = ymd_hms(modification_time),
    # create the photo filenames for renaming
    pheno_name = glue("{cam_site}_{format(as_date(datetime), '%Y%m%d')}_{gsub(':', '', hms::as_hms(datetime))}.{path_ext(file_name)}"),
    hashid = map_vec(path, ~digest::digest(.x, algo="crc32", serialize=FALSE)),
    pheno_name_uniq = glue("{cam_site}_{format(as_date(datetime), '%Y%m%d')}_{gsub(':', '', hms::as_hms(datetime))}_{hashid}.{path_ext(file_name)}"))

# make sure all photos have a unique name! (Below should return zero)
photo_list |> group_by(pheno_name_uniq) |> tally() |> filter(n>1) |> nrow()

# Preferred: Rename in Place Unique -----------------------------------------

# rename photos in place (from same location you read them in)
# using unique hash code appended to site_date_time
fs::file_move(path = photo_list$path, new_path = glue("{fs::path_dir(photo_list$path)}/{photo_list$pheno_name_uniq}"))

# Rename in Place Pheno  -----------------------------------------------------

# For pheno timelapse we can rename in place using a
# simple pheno_name assuming there are nu duplicates (photos at 15min or hourly interval)
fs::file_move(path = photo_list$path, new_path = glue("{fs::path_dir(photo_list$path)}/{photo_list$pheno_name}"))


