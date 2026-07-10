# iphone workup
# to save and resave audio files from iphone

library(lubridate)

# specs for file naming
device <- "Ryan's iPhone"
mic <- "internal mic"
app <- "Voice Memos"

# get names of all m4a files in folder
# the m4a files copied to this folder will all be renamed when this script is run
myfolder <- "change name/"
myFiles <- list.files(path= myfolder, pattern="*.m4a")

for (f in c(1:length(myFiles))){
  # loop through each file, get its creation date, then rename
  # note that R's file.info function doesn't get the creation date for files (at least not on Mac)
  # instead I need to ask the Terminal for this info
  # this is what I used to do before Apple's sharing set the file creation date to the date the file was shared
  # terminalCode <- paste0 ("stat -f'%SB' -t \"%Y-%m-%d %H-%M-%S\" \"", normalizePath(paste0(myfolder, myFiles[f])), "\"")
  terminalCode <- paste0 ("exiftool \"", normalizePath(paste0(myfolder, myFiles[f])), "\"")
  exiftoolOutput <- system (terminalCode, intern=TRUE) #intern=TRUE brings the result into R
  MediaCreateDate <- exiftoolOutput [grepl("Media Create Date.+", exiftoolOutput)]
  creationDate <- sub ("Media Create Date[ ]+:[ ]+([0-9]{4}):([0-9]{2}):([0-9]{2}) ([0-9]+):([0-9]{2}):([0-9]{2})", "\\1-\\2-\\3", MediaCreateDate)
  creationTime <- sub ("Media Create Date[ ]+:[ ]+([0-9]{4}):([0-9]{2}):([0-9]{2}) ([0-9]+):([0-9]{2}):([0-9]{2})", "\\4:\\5:\\6", MediaCreateDate)

  # This is UTC. Now adjust for time zone.
  DateTimeUTC <- as.POSIXlt (paste(creationDate, creationTime), tz = "UTC")
  DateTimeNZT <- with_tz(DateTimeUTC, "Pacific/Auckland") # lubridate applies daylight savings time when appropriate, so the output will be either NZST or NZDT (it will include whichever of those codes is appropriate)

  file.rename (from = paste0(myfolder, myFiles[f]), to = gsub(":", "-", paste0(myfolder, sub ("[A-Z]+", "", DateTimeNZT), " ",  device, "-", mic, "-", app,".m4a")))

}
