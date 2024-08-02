# rename photos sequentially by date including site_id
# this is important to do before uploading to Wildlife Insights.
# it can be done directly on the SD card or if photos are copied to a directory on the computer


# Libraries ---------------------------------------------------------------

library(tidyverse) # various naming/tidying functions
library(glue) # pasting names together
library(janitor) # cleaning filenames
library(fs) # file/directory metadata
library(exiftoolr) # getting metadata from photos

# Set Paths ---------------------------------------------------------------

# CHANGE/CHECK THESE!

username <- Sys.getenv("USERNAME")
drive <- r'(C:\Users\)'
onedrive <- r'(OneDrive - California Department of Fish and Wildlife\)'
year <- 2024

# Site ID (match exactly)
cam_site <- "WOMY4"

# Full path to folder where photos are located (change drive letter if directly from SD)
photo_directory <- r'(F:\DCIM\104RECNX)'

# Getting Exiftools Installed ---------------------------------------------

# this is what is used to extract metadata: Only needed for exposure/extensive metadata
# this process only needs to be done ONCE

# 1. Download the ExifTool Windows Executable (stand-alone) version (.zip) from here:
## https://exiftool.org/, look for Windows Executable: exiftool-xx.xx.zip
## Download to default downloads folder (i.e., Downloads/exiftool-12.87.zip)

# 2. create path to the downloaded tool:

# make sure the version number matches what you have
# (different versions should still work, just need the correct one below for the path to work)
# path_to_exif_zip <- glue("{drive}/{username}/Downloads/exiftool-12.87.zip")

# check path works!
# if(fs::file_exists(path_to_exif_zip)=="TRUE") {
#   print("Path is legit!")
# } else( "Path is borked...double check")

# 3. Now we can test and install

# this only needs to be done once!
# if(is.null(exif_version())){
#   install_exiftool(local_exiftool = path_to_exif_zip)
#   print("Installed!")
# } else( "already installed!")

# make sure it works:
# if(!is.null(exif_version())){
#   glue("exiftools successfully installed! \n Version: {exif_version()}")
# } else( "exiftools is not installed, check that path is correct and rerun install_exiftool()")

# Get Photo File List -----------------------------------------------------

# get a complete list of all the photos on the card or in the directory
# note recurse=FALSE, no nested folders can be present
photo_list <- fs::dir_info(photo_directory, type = "file", recurse = FALSE)

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

# make sure all unique! (should be zero)
photo_list |> group_by(pheno_name_uniq) |> tally() |> filter(n>1) |> nrow()

# Rename Photos -----------------------------------------------------------

# rename photos in place (from same location you read them in)
# using unique hash code appended to site_date_time
fs::file_move(path = photo_list$path, new_path = glue("{fs::path_dir(photo_list$path)}/{photo_list$pheno_name_uniq}"))

# or for pheno timelapse, can use this to stick with simple pheno_name assuming no dups
#fs::file_move(path = photo_list$path, new_path = glue("{fs::path_dir(photo_list$path)}/{photo_list$pheno_name}"))


# PHENO: Get Photo METADATA  -------------------------------------------------

## REQUIRES exiftoolr
## with large photo lists, this can take a few (3+) minutes.
## gets metadata and creates a columns with filenames that match phenocam requirements
## and is used to rename files "pheno_name"

# this takes awhile with lots of photos
if(nchar(exiftoolr::exif_version())>0){ # fails here if exif not installed
  photo_attribs <- exiftoolr::exif_read(photo_list$path[1:100])  |>
    # reformat and select fields of interest
    janitor::clean_names() |>
    select(file_path=directory, file_name, file_number,
           datetime = create_date, exposure=exposure_time,
           moon_phase:ambient_temperature, ambient_infrared,
           ambient_light, image_size, image_width, image_height,
           battery_voltage_avg) |>
    mutate(datetime = ymd_hms(datetime),
           # create the photo filenames for renaming
           pheno_name = glue("{cam_site}_{format(as_date(datetime), '%Y%m%d')}_{gsub(':', '', hms::as_hms(datetime))}.{path_ext(file_name)}"),
           pheno_name_uniq = glue("{cam_site}_{format(as_date(datetime), '%Y%m%d')}_{gsub(':', '', hms::as_hms(datetime))}_{file_number}.{path_ext(file_name)}"))
} else("No exiftools installed...")


# PHENO: Write IMG Metadata ----------------------------------------------

# create path to drive location:
#metadata_path <- glue("{drive}/{username}/{onedrive}/Terrestrial/Data/{year}/Timelapse_Camera")

# write out metadata for later if we want it
#write_csv(photo_attribs, glue("{metadata_path}/{cam_site}_pheno_img_metadata.csv.gz"))

