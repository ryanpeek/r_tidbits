# set pin board for use/sharing:

# load library
library(pins)

# can make temp (only accessible in session):
#board <- board_temp()
# then write/read:
#board %>% pin_write(head(mtcars), "mtcars")

# make more permanent (shared option):

# make a board:
board <- board_folder(path = r'(C:\Users\RPeek\OneDrive - California Department of Fish and Wildlife\Terrestrial Species Monitoring\Branch\2023\DroughtSpecies\data_output\)')

# get data to use
library(readxl)
library(dplyr)

# data
dat <- read_xlsx(path = r'(C:\Users\RPeek\OneDrive - California Department of Fish and Wildlife\Terrestrial Species Monitoring\Branch\2023\DroughtSpecies\data_output\2023_Drought_List_Birds_Herps_Mammals_v12.xlsx)', sheet = 'DATA')


# write pin
pins::pin_write(board, dat, name = "drought_gentime")

# read pin
df <- pins::pin_read(board, "drought_gentime")
