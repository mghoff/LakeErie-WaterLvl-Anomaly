# Clear Environment, set working directory, get data, load libraries
# setwd("C:/path/to/folder") # Uncomment this to set your working directory.
rm(list=ls())
gl_wtr <- read.csv("http://lre-wm.usace.army.mil/ForecastData/GLHYD_data_english.csv",
                   stringsAsFactors = F, skip = 12, header = T)
View(gl_wtr)

library(ggplot2)
library(reshape2)
library(lubridate)
library(scales)
library(viridis)
library(dplyr)

# Tidy Data, Calculate Long Term Monthly Average
erie_wtr <- gl_wtr[,c(1:2,6)]
monthly_avg <- erie_wtr %>%
     group_by(month) %>%
     summarise(monthly.avg = mean(Erie))

# Join Original Data with LT Monthly Avg & Calculate Each Month's Anomaly
erie_wtr <- left_join(erie_wtr, monthly_avg, by = c("month"))
erie_wtr$anom <- erie_wtr$Erie - erie_wtr$monthly.avg

# Create a Date
erie_wtr$date <- as.Date(paste(erie_wtr$year, erie_wtr$month, "01"), "%Y %b %d")
# View(erie_wtr)

# Plot the Anomaly Data
ggplot(erie_wtr, aes(y=month(date), x=year(date)))+
     geom_tile(aes(fill=anom))+
     scale_fill_viridis(option="inferno")+
     scale_y_reverse(breaks=1:12, labels=strftime(paste("0001-",1:12,"-01",sep=""), "%b"))+
     scale_x_continuous(breaks=seq(1918, 2018, 10))+
     labs(title="Lake Erie Water Level Anomaly",
          subtitle="source: http://www.lre.usace.army.mil/",
          x="",y="",
          fill="Difference\nFrom the\nLong Term\nMean (Feet)",
          caption="Created by mghoff")+
     theme_bw()+
     theme(panel.grid.minor = element_blank())

# Save
# ggsave("~/Erie-AVG.png", height=5, width=12.5, dpi=120, type="cairo-png")
