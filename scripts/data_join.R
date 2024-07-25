# get data and join

library(readxl)
library(tidyverse)
library(glue)

# Set Paths on Computer ---------------------------------------------------

# paths to DB
username <- Sys.getenv("USERNAME")
drive <- r'(C:/Users/)'
folder_file_lives <- "Downloads"

# make a full path with glue
path_to_files <- glue("{drive}/{username}/{folder_file_lives}")

# Get Data ----------------------------------------------------------------

# list of cities
#cities <- read_xlsx(glue("{path_to_files}/JLD City Inclusion List.xlsx"))

# data
job_data <- read_xlsx(glue("{path_to_files}/FISAP Data 2324.xlsx"), sheet=1)

# list of cities
cities <- read_xlsx(glue("{path_to_files}/FISAP Data 2324.xlsx"), sheet = 2)



# What Columns Can we Join with? ------------------------------------------

names(job_data) # gives column names
# so "Locations City" and "City" are the fields we'll want

# let's clean up column names to make this easier
# general rules to make data wrangling on a computer easier:
## avoid spaces
## avoid special characters (things like "*?())

# good news we can use a function to clean the names for us really quickly:
library(janitor) # helps with cleaning data

job_data_clean <- clean_names(job_data)
names(job_data_clean) # great much nicer to type!

# do same for cities:
cities_clean <- clean_names(cities)


# Join Data ---------------------------------------------------------------

# many different joins, but left join means
# keep all the stuff from x, and only keep matches in y

job_data_filtered <- left_join(cities_clean, job_data_clean,
                               # since col names are different
                               # we need to specify
                               by=c("city"="locations_city"))


# use this to check all these are cities in CA
table(job_data_filtered$locations_state)
# hmmm...so that means some cities are shared across states.
# easy to fix!

job_data_filtered_ca <- job_data_filtered |>
  filter(locations_state == "California")

# better!
table(job_data_filtered_ca$locations_state)

# let's get the remote jobs only
remote_jobs <- job_data_clean |>
  filter(jobs_remote_yes_no == "Yes")
table(remote_jobs$jobs_remote_yes_no)


# Write it back out! ------------------------------------------------------

# now we have the data, let's export it back out

# create the location we want to save it:
out_path <- glue("{drive}/{username}/{folder_file_lives}")
out_path

# get the date that can be used to datestamp the file
datestamp <- gsub("-", "", Sys.Date())

# now write it out
write_csv(remote_jobs, file = glue("{out_path}/remote_jobs_{datestamp}.csv"))

# now ca only
write_csv(job_data_filtered_ca, file = glue("{out_path}/jobs_in_select_ca_locations_{datestamp}.csv"))
