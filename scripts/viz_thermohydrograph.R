# thermohydrograph
library(tidyverse)
library(dataRetrieval)
library(viridis)
library(janitor)
library(gridExtra)

# Get Data ----------------------------------------------------------------

siteNumber <- "01491000"
parameterCd <- c("00010", "00060") # Temperature and discharge
statCd <- c("00003") # Mean and maximum
startDate <- "2022-10-01"
endDate <- "2023-09-30"

dff <- readNWISdv(siteNumber, parameterCd,
                  startDate, endDate,
                  statCd = statCd)

dff <- readNWISuv(siteNumber, parameterCd,
                  startDate, endDate)

dff <- dataRetrieval::addWaterYear(dff)
dff <- dataRetrieval::renameNWISColumns(dff)

dff <- clean_names(dff)
dff$datetime <- dff$date

dff <- filter(dff, !is.na(wtemp))

# Make Test Thermohydro Plots --------------------------------------------------------------

## SET COLORS AND BREAKS FOR WATER TEMPERATURES
#breaks<-(seq(0,36,4)) # best for air
breaks<-(seq(0,27,3)) # best for water

palette<-c("black","midnightblue","blue","deepskyblue2",
                  "green4","green","yellow","orange","red","darkviolet") # orange orangered brown4

## plot stage or flow
usgslab<-"Q (cfs)" # "Stage (m)"
minusgs<-min(dff$flow) # or stage_m

ggplot() + geom_line(data = dff, aes(x=date, y=flow, color=wtemp), size=2) +
  scale_color_viridis_c(alpha = 0.8, name=expression(paste("Water \nTemp (", degree, "C)")), option = "A")+
  #scale_color_gradientn(name=expression(paste("WaterTemp(",degree,"C)")),
  #                       colors=palette(palette), values = breaks, limits = range(breaks),
  #                       rescaler = function(x, ...) x) +
  #geom_ribbon(data = dff[dff$date <= dff$date[nrow(dff)],],
  #            aes(x = date, ymax = flow, ymin = minusgs), fill = "gray80", alpha = .5) +
  labs(title = "Thermohydrograph", x="",y = usgslab) + hrbrthemes::theme_ipsum_rc()

