
library(stringr)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/Station_1")

station_1 <- read_csv("Masdar_Station_1.csv")
colnames(station_1) <- c("date", "PM25", "PM10", "PM1", "TSP")


station_1 <- station_1 %>%
  mutate(date = ymd_hm(date))



station_1 <- gather(station_1, PM, value, 2:5)

# Overlaid histograms
ggplot(station_1, aes(value, fill = PM)) +
  geom_histogram(binwidth=.5, alpha=.5,position="identity") +
  theme_bw() +
  xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
  ylab("count") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
  xlim(0, 300) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
  ggtitle(expression(paste("Distribution of particles (Masdar Stn. 1)"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))



# Overlaid histograms
ggplot(station_1, aes(value, fill = PM)) +
  geom_density(alpha=.5,position="identity") +
  theme_bw() +
  xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
  ylab("count") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
  xlim(0, 300) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
  ggtitle(expression(paste("Distribution of particles (Masdar Stn. 1)"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))




