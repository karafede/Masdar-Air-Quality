

library(stringr)
library(leaflet)
library(NISTunits)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain")


###################################################
######### plot TIME-SERIES of MODIS AQUA DATA #####
###################################################

# AOD in Umm Al Quwain ######
# DAILY DATA ################


AQUA_AOD <- read.csv("MODIS_AQUA_AOD_Daily_Umm.csv")
str(AQUA_AOD)

# remove -999 value

AQUA_AOD <- AQUA_AOD[!AQUA_AOD$AOD ==-9999, ]

# change time format and generate PM10 and PM2.5

AQUA_AOD <- AQUA_AOD %>%
  mutate(Date = mdy(Date),
         PM25 = 94*AOD,
         PM10 = 294*AOD)
write.csv(AQUA_AOD, "PM_Umm_Al_Quwain.csv")

AQUA_AOD$Date <- as.Date(AQUA_AOD$Date)

min <- as.Date("2013-01-01") 
max <- as.Date("2017-11-30") 

AQUA_AOD <- AQUA_AOD %>%
  filter(Date > "2013-01-01")


plot <- ggplot(AQUA_AOD, aes(Date, PM10)) +
  theme_bw() +
  geom_line(aes(y = PM10, col = "PM10"), alpha=1, col="black") +
  geom_line(aes(y = PM25, col = "PM25"), alpha=1, col="red") +
  #  scale_color_discrete(name = "Y series", labels = c("PM10", "PM2.5")) +
  # stat_smooth(method = "loess") +
  # facet_grid(Pollutant ~ .) +
  # facet_wrap(~ Pollutant, nrow = 2) +
  theme(strip.text = element_text(size = 10)) + 
  # ylab(expression(paste("Monthly concentration (add units)"))) +
  ylab(expression(paste(PM[2.5]," & ", PM[10], " (µg/",m^3, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  scale_x_date(breaks = pretty_breaks(10)) +
  ylim(0, 500) 
  # xlim(min, max)
plot


#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/"


png(paste0(output_folder,"Daily_Time_series_PM10_PM25_MODIS_AQUA.png"), 
    width = 1400, height = 600,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()

#############################################
#############################################

##################
### PM10 #########
##################

plot <- ggplot(AQUA_AOD, aes(Date, PM10)) +
  theme_bw() +
  geom_line(aes(y = PM10, col = "PM10"), alpha=1, col="black") +
#  geom_line(aes(y = PM25, col = "PM25"), alpha=1, col="red") +
  #  scale_color_discrete(name = "Y series", labels = c("PM10", "PM2.5")) +
  # stat_smooth(method = "loess") +
  # facet_grid(Pollutant ~ .) +
  # facet_wrap(~ Pollutant, nrow = 2) +
  theme(strip.text = element_text(size = 10)) + 
  # ylab(expression(paste("Monthly concentration (add units)"))) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  scale_x_date(breaks = pretty_breaks(10)) +
  geom_hline(yintercept = 150, color = "red", size =1) +
  ggtitle("Daily PM10 concentration (Umm Al Quwain 2013-2017)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 12, hjust = 0.5)) +
  ylim(0, 500) 
# xlim(min, max)
plot

png(paste0(output_folder,"Daily_Time_series_PM10_MODIS_AQUA.png"), 
    width = 1400, height = 600,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()




##################
### PM2.5 ########
##################

plot <- ggplot(AQUA_AOD, aes(Date, PM25)) +
  theme_bw() +
  geom_line(aes(y = PM25, col = "PM25"), alpha=1, col="red") +
  #  geom_line(aes(y = PM25, col = "PM25"), alpha=1, col="red") +
  #  scale_color_discrete(name = "Y series", labels = c("PM10", "PM2.5")) +
  # stat_smooth(method = "loess") +
  # facet_grid(Pollutant ~ .) +
  # facet_wrap(~ Pollutant, nrow = 2) +
  theme(strip.text = element_text(size = 10)) + 
  # ylab(expression(paste("Monthly concentration (add units)"))) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  scale_x_date(breaks = pretty_breaks(10)) +
  geom_hline(yintercept = 35, color = "black", size =1) +
  ggtitle("Daily PM2.5 concentration (Umm Al Quwain 2013-2017)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 12, hjust = 0.5)) +
  ylim(0, 100) 
# xlim(min, max)
plot

png(paste0(output_folder,"Daily_Time_series_PM2.5_MODIS_AQUA.png"), 
    width = 1400, height = 600,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()



#################################################################################
################ Monthly averages ###############################################
#################################################################################


AQUA_AOD <- read.csv("MODIS_AQUA_AOD_Daily_Umm.csv")
str(AQUA_AOD)

# remove -999 value

AQUA_AOD <- AQUA_AOD[!AQUA_AOD$AOD ==-9999, ]

# change time format and generate PM10 and PM2.5

AQUA_AOD <- AQUA_AOD %>%
  mutate(Date = mdy(Date),
         PM25 = 94*AOD,
         PM10 = 294*AOD)

AQUA_AOD <- AQUA_AOD %>%
  mutate(MONTH = month(Date),
         YEAR = year(Date))


AQUA_AOD <- AQUA_AOD %>%
  group_by(YEAR,
           MONTH) %>%
  summarise(PM25_mean = mean(PM25),
            PM10_mean = mean(PM10))



AQUA_AOD$Date <- paste0(AQUA_AOD$YEAR,"-", AQUA_AOD$MONTH, "-", "01")
str(AQUA_AOD)


AQUA_AOD <- AQUA_AOD %>%
  mutate(Date = ymd(Date))


##################
### PM10 #########
##################

plot <- ggplot(AQUA_AOD, aes(Date, PM10_mean)) +
  theme_bw() +
  geom_line(aes(y = PM10_mean, col = "PM10_mean"), alpha=1, col="black") +
  #  geom_line(aes(y = PM25, col = "PM25"), alpha=1, col="red") +
  #  scale_color_discrete(name = "Y series", labels = c("PM10", "PM2.5")) +
  # stat_smooth(method = "loess") +
  # facet_grid(Pollutant ~ .) +
  # facet_wrap(~ Pollutant, nrow = 2) +
  theme(strip.text = element_text(size = 10)) + 
  # ylab(expression(paste("Monthly concentration (add units)"))) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  scale_x_date(breaks = pretty_breaks(10)) +
  geom_hline(yintercept = 150, color = "red", size =1) +
  ggtitle("Monthly PM10 concentration (Umm Al Quwain 2007-2017)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 12, hjust = 0.5)) +
  ylim(0, 300) 
# xlim(min, max)
plot


png(paste0(output_folder,"Monthly_Time_series_PM10_MODIS_AQUA.png"), 
    width = 1400, height = 600,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()



##################
### PM2.5 ########
##################

plot <- ggplot(AQUA_AOD, aes(Date, PM25_mean)) +
  theme_bw() +
  geom_line(aes(y = PM25_mean, col = "PM25_mean"), alpha=1, col="red") +
  #  geom_line(aes(y = PM25, col = "PM25"), alpha=1, col="red") +
  #  scale_color_discrete(name = "Y series", labels = c("PM10", "PM2.5")) +
  # stat_smooth(method = "loess") +
  # facet_grid(Pollutant ~ .) +
  # facet_wrap(~ Pollutant, nrow = 2) +
  theme(strip.text = element_text(size = 10)) + 
  # ylab(expression(paste("Monthly concentration (add units)"))) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  scale_x_date(breaks = pretty_breaks(10)) +
  geom_hline(yintercept = 35, color = "black", size =1) +
  ggtitle("Monthly PM2.5 concentration (Umm Al Quwain 2007-2017)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 12, hjust = 0.5)) +
  ylim(0, 100) 
# xlim(min, max)
plot


png(paste0(output_folder,"Monthly_Time_series_PM2.5_MODIS_AQUA.png"), 
    width = 1400, height = 600,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()



#########################################################################################
#########################################################################################
#########################################################################################

##### summary statistics of Daily (24h data) ##################
###############################################################

AQUA_AOD <- read.csv("MODIS_AQUA_AOD_Daily_Umm.csv")
str(AQUA_AOD)

# remove -999 value

AQUA_AOD <- AQUA_AOD[!AQUA_AOD$AOD ==-9999, ]

# change time format and generate PM10 and PM2.5

AQUA_AOD <- AQUA_AOD %>%
  mutate(Date = mdy(Date),
         PM25 = 94*AOD,
         PM10 = 294*AOD)

AQUA_AOD <- AQUA_AOD %>%
  mutate(MONTH = month(Date),
         YEAR = year(Date))



## get the months of observations
AQUA_AOD$month <- factor(format(AQUA_AOD$Date, format = "%b"), levels = month.abb)



## Define seasons
AQUA_AOD$season <- character(length = nrow(AQUA_AOD))
AQUA_AOD$season[AQUA_AOD$month %in% month.abb[c(1:2)]] <- "winter"
AQUA_AOD$season[AQUA_AOD$month %in% month.abb[c(12)]] <- "winter"
AQUA_AOD$season[AQUA_AOD$month %in% month.abb[c(3:5)]] <- "spring"
AQUA_AOD$season[AQUA_AOD$month %in% month.abb[c(6:8)]] <- "summer"
AQUA_AOD$season[AQUA_AOD$month %in% month.abb[c(9:11)]] <- "fall"
AQUA_AOD$season <- factor(AQUA_AOD$season, levels = c("winter","spring","summer","fall"))



AQUA_AOD$YEAR <- as.factor(AQUA_AOD$YEAR)
str(AQUA_AOD)


############
## PM2.5 ###
############

plot <- ggplot(AQUA_AOD, aes(YEAR, PM25, fill = YEAR)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 150) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=18, colour = "black")) +
  ggtitle("annual distribution of daily PM2.5 concentration (Umm Al Quwain)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 16, hjust = 0.5)) +
  geom_hline(yintercept =35, col="#00CD00", lty=1, size = 1)
  
plot





# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/"

png(paste0(output_folder,"Daily_stats_PM2.5_MODIS_AQUA.png"), 
    width = 1700, height = 800,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()




################
## PM10 ########
################


plot <- ggplot(AQUA_AOD, aes(YEAR, PM10, fill = YEAR)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 200) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=18, colour = "black")) +
  ggtitle("annual distribution of daily PM10 concentration (Umm Al Quwain)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 16, hjust = 0.5)) +
  geom_hline(yintercept =150, col="#00CD00", lty=1, size = 1)

plot





# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/"

png(paste0(output_folder,"Daily_stats_PM10_MODIS_AQUA.png"), 
    width = 1700, height = 800,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()




#######################################################################################
##### seasonal Plots ##################################################################
#######################################################################################

# gather data...PM10 and PM2.5 into one column


new_data <- gather(AQUA_AOD, "Pollutant", "concentration", 3:4)

plot <- ggplot(new_data, aes(YEAR, concentration, fill = Pollutant)) +
  theme_bw() +
  geom_boxplot() + 
  facet_grid(season ~ .) +
  # guides(fill=FALSE) +   # no legend
  ylim(0, 300) +
  theme( strip.text = element_text(size = 14)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
  ylab(expression(paste(PM, " (µg/",m^3, ")"))) +
 # xlab(expression("dominant pollutant")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=10),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour="black")) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=14, colour="black")) +
  ggtitle("annual distribution of daily PM2.5 & PM10 concentration (Umm Al Quwain)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 12, hjust = 0.5)) +
  geom_hline(yintercept =150, col="#0000ff", lty=1, size = 0.8) +
  geom_hline(yintercept =35, col="#00CD00", lty=1, size = 0.8)
plot
  
  
# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/"

png(paste0(output_folder,"Daily_stats_PM_season_MODIS_AQUA.png"), 
    width = 2000, height = 1000,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()








#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################

#############################################
######### plot TIME-SERIES of MERRA DATA ####
#############################################

# AOD in Umm Al Quwain ###
# Monthly data ###########


MERRA_AOD <- read.csv("MERRA_AOD_Umm.csv")
str(MERRA_AOD)

# change time format and generate PM10 and PM2.5

MERRA_AOD <- MERRA_AOD %>%
  mutate(Date = mdy_hm(DateTime),
         PM25 = 94*AOD,
         PM10 = 294*AOD)

MERRA_AOD$Date <- as.Date(MERRA_AOD$Date)

# MERRA_AOD <- gather(MERRA_AOD, "Pollutant", "concentration", 4:5)


plot <- ggplot(MERRA_AOD, aes(Date, PM10)) +
  theme_bw() +
  geom_line(aes(y = PM10, col = "PM10"), alpha=1, col="black") +
  geom_line(aes(y = PM25, col = "PM25"), alpha=1, col="red") +
#  scale_color_discrete(name = "Y series", labels = c("PM10", "PM2.5")) +
  stat_smooth(method = "loess") +
 # facet_grid(Pollutant ~ .) +
 # facet_wrap(~ Pollutant, nrow = 2) +
  theme(strip.text = element_text(size = 10)) + 
 # ylab(expression(paste("Monthly concentration (add units)"))) +
  ylab(expression(paste(PM[2.5]," & ", PM[10], " (µg/",m^3, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  scale_x_date(breaks = pretty_breaks(15)) +
  ylim(0, 220)
plot


#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/"


png(paste0(output_folder,"Mothly_Time_series_PM10_PM25_MERRA.png"), 
    width = 1400, height = 600,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


################################################



