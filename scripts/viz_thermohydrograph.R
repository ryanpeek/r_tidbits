# thermohydrograph

# Make Test Thermohydro Plots --------------------------------------------------------------

## SET COLORS AND BREAKS FOR WATER TEMPERATURES
breaksair<-(seq(0,36,4)) # best for air
breaksh20<-(seq(0,27,3)) # best for water

palette<-c("black","midnightblue","blue","deepskyblue2",
                  "green4","green","yellow","orange","red","darkviolet") # orange orangered brown4

# USGS plot with air as thermoscale

## plot stage or flow
water<-"flow_cms" #  "stage_m"
usgslab<-"Q (cms)" # "Stage (m)"
minusgs<-min(dff$flow_cms) # or stage_m

grid.arrange(
  ggplot() + # can switch flow_cms with stage_m and use air for color
    geom_line(data = dff, aes_string(x = "datetime", y = water), color = "grey50", size = 1, alpha=0.8) +
    scale_colour_gradientn(name=expression(paste("AirTemp(",degree,"C)")),colours=palette(palette), values=breaksair,
                           rescaler = function(x, ...) x, oob = identity,limits=range(breaksair), breaks=breaksair, space="Lab") +
    geom_ribbon(data = dff[dff$datetime <= dff$datetime[nrow(dff)],],
                aes_string(x = "datetime", ymax = water, ymin = minusgs), fill = "gray80", alpha = .5) +
    geom_line(data = dff[dff$datetime <= dff$datetime[nrow(dff)],],
              aes_string(x = "datetime", y = water, color="air_C"), size = 1) +
    geom_point(data = dff[dff$datetime == dff$datetime[nrow(dff)],],
               aes_string(x = "datetime", y = water), pch=21, fill="gray10", col="white",size = 8) +
    labs(list(x="",y = usgslab)) + theme_bw() +
    theme(axis.text.x = element_text(face="bold", size=12),
          axis.text.y = element_text(face="bold", size=12),
          axis.title.y = element_text(face="bold", size=12),
          panel.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
)
