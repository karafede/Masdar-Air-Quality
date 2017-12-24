
library(dplyr)
library(leaflet)
library(readr)
library(lubridate)

#################################################
# save big data only with .Rdata format #########
#################################################

################################################################################
## AIR QUALITY INDEXES ##-------------------------------------------------------
################################################################################


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain")

# load daily sadellite derived measurment for PM10 and PM2.5
PM_data <- read.csv("PM_Umm_Al_Quwain.csv")

PM_data <- PM_data %>%
  mutate(Date = ymd(Date))

str(PM_data)

# load function for AQI calcualtions

# source("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/aqi_fun.R")
# source("D:/AQI/aqi_fun.R")
source("D:/AQI/aqi_fun_UAE.R")


###########  
# PM2.5 ###
###########


PM25_data <- as.vector(PM_data$PM25)
PM25_data <- as.numeric(PM25_data)

# calculate Air Quality index for PM2.5
aqi_PM25 <- lapply(PM25_data, aqi_PM25_fun)

aqi_PM25 <- as.numeric(aqi_PM25)
aqi_PM25 <- as.data.frame(aqi_PM25)
  
  
##########
## PM10 ##
##########
  
PM10_data <- as.vector(PM_data$PM10)

# calculate Air Quality index for PM10
aqi_PM10 <- lapply(PM10_data, aqi_PM10_fun)

aqi_PM10 <- as.numeric(aqi_PM10)
aqi_PM10 <- as.data.frame(aqi_PM10)
  
 
AQ_data_AQI <- cbind(PM_data, aqi_PM25, aqi_PM10)

save(AQ_data_AQI, file="Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/AQ_data_AQI_24h.Rdata")

##########################################################################
##########################################################################

# find maximum AQI in each row

# restart R

rm(list=ls())

library(dplyr)
library(leaflet)
library(readr)
library(lubridate)

load("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/AQ_data_AQI_24h.Rdata")


all_AQI <- AQ_data_AQI %>%
  dplyr::select(aqi_PM25,
                aqi_PM10)

str(all_AQI)
  
# all_AQI$aqi_PM25 <- as.numeric(all_AQI$aqi_PM25)


# Boxplot of 
boxplot(all_AQI) 
head(all_AQI)

# all_AQI <- all_AQI[2,]


# function to find the Pollutant with the maximum AQI

max_fe <- function(fed){
  if(all(is.na(fed))){
    daw<- NA
  }else{
    daw<- max(fed, na.rm = TRUE)
  }
  return(daw)
}


# function to find the Pollutant with the count for Pollutant with max AQI

max_pollu <- function(fed){
  if(all(is.na(fed))){
    daw<- NA
    # o3 = 0
    # co = 0
    pm25 = 0
    pm10 = 0
    # so2 = 0
    # no2 = 0
 #   count_line<- c(daw,o3,co,pm25,pm10,so2,no2)
 #   count_line<- c(daw,pm25,pm10)
  }else{
    chk_1<- max(fed, na.rm = TRUE)
    federico<-fed==chk_1
    # o3 = 0
    # co = 0
    pm25 = 0
    pm10 = 0
    # so2 = 0
    # no2 = 0
    indi_max<-which(federico)
    # if(indi_max==1){
    #   daw<-"O3"
    #   o3<-1
    # }
    # if(indi_max==2){
    #   daw<-"CO"
    #   co<-1
    # }
    if(indi_max==1){
      daw<-"PM25"
      pm25<-1
    }
    if(indi_max==2){
      daw<-"PM10"
      pm10<-1
    }
    # if(indi_max==5){
    #   daw<-"SO2"
    #   so2<-1
    # }
    # if(indi_max==6){
    #   daw<-"NO2"
    #   no2<-1
    # }
  }
#  count_line<- (c(daw,o3,co,pm25,pm10,so2,no2))
  count_line<- (c(daw,pm25,pm10))
  
  return(count_line)
}

# fed<-all_AQI[20,1:6]

# max_pollu(all_AQI[20,])

# attach the maximum
all_AQI$max_AQI <- apply(all_AQI[,1:2], 1, max_fe)
head(all_AQI)

new_data <-data.frame()
new_data <- apply(all_AQI[,1:2], 1, max_pollu)
new_data <- t(new_data)
new_data <- as.data.frame(new_data)

# all_AQI$max_Pollutant <- apply(all_AQI[,1:6], 1, max_pollu)
# all_AQI$max_AQI <- apply(all_AQI, 1, max, na.rm = TRUE)

AQ_data_AQI <- cbind(AQ_data_AQI, all_AQI[,3],new_data)
head(AQ_data_AQI)
# rename columns
names(AQ_data_AQI)[names(AQ_data_AQI) == 'all_AQI[, 3]'] <- 'max_AQI'
names(AQ_data_AQI)[names(AQ_data_AQI) == 'V1'] <- 'max_Pollutant'
# names(AQ_data_AQI)[names(AQ_data_AQI) == 'V2'] <- 'count_O3'
# names(AQ_data_AQI)[names(AQ_data_AQI) == 'V3'] <- 'count_CO'
names(AQ_data_AQI)[names(AQ_data_AQI) == 'V2'] <- 'count_PM25'
names(AQ_data_AQI)[names(AQ_data_AQI) == 'V3'] <- 'count_PM10'
# names(AQ_data_AQI)[names(AQ_data_AQI) == 'V6'] <- 'count_SO2'
# names(AQ_data_AQI)[names(AQ_data_AQI) == 'V7'] <- 'count_NO2'
head(AQ_data_AQI)


rm(all_AQI, new_data)

# write_csv(AQ_data_AQI, "D:/AQI/AQ_data_AQI_counts_new.csv")
save(AQ_data_AQI,file="Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/AQ_data_AQI_counts_24h.Rdata")


####################################################################################

library(dplyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain")

### read data
# AQ_data_AQI<- read_csv("D:/AQI/AQ_data_AQI_counts_new.csv")
# AQ_data_AQI <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQI/AQ_data_AQI_counts.csv")
load("AQ_data_AQI_counts_24h.Rdata")

head(AQ_data_AQI)



# get the maximum AQI ###################
AQ_data_AQI_max <- AQ_data_AQI %>%
  group_by(Date) %>%
  summarize(max_AQI = max(max_AQI))

head(AQ_data_AQI_max)

AQ_data_AQI_Pollutant <- AQ_data_AQI_max %>%
  left_join(AQ_data_AQI, by = c("Date", "max_AQI"))

head(AQ_data_AQI_Pollutant)

## get the months of observations
AQ_data_AQI_Pollutant$month <- factor(format(AQ_data_AQI_Pollutant$Date, format = "%b"), levels = month.abb)

rm(AQ_data_AQI)
rm(AQ_data_AQI_max)

head(AQ_data_AQI_Pollutant)
str(AQ_data_AQI_Pollutant)


## Define seasons
AQ_data_AQI_Pollutant$season <- character(length = nrow(AQ_data_AQI_Pollutant))
AQ_data_AQI_Pollutant$season[AQ_data_AQI_Pollutant$month %in% month.abb[c(1:2)]] <- "winter"
AQ_data_AQI_Pollutant$season[AQ_data_AQI_Pollutant$month %in% month.abb[c(12)]] <- "winter"
AQ_data_AQI_Pollutant$season[AQ_data_AQI_Pollutant$month %in% month.abb[c(3:5)]] <- "spring"
AQ_data_AQI_Pollutant$season[AQ_data_AQI_Pollutant$month %in% month.abb[c(6:8)]] <- "summer"
AQ_data_AQI_Pollutant$season[AQ_data_AQI_Pollutant$month %in% month.abb[c(9:11)]] <- "fall"
AQ_data_AQI_Pollutant$season <- factor(AQ_data_AQI_Pollutant$season, levels = c("winter","spring","summer","fall"))

str(AQ_data_AQI_Pollutant)

head(AQ_data_AQI_Pollutant)


# remove lines wtih NA in the max_AQI column
AQ_data_AQI_Pollutant <- AQ_data_AQI_Pollutant[!is.na(AQ_data_AQI_Pollutant$max_AQI),]


head(AQ_data_AQI_Pollutant)

# AQ_data_AQI_Pollutant <- na.omit(AQ_data_AQI_Pollutant)

# ADD YEAR AS VARIABLE ##---------------------------------------------------
AQ_data_AQI_Pollutant <- AQ_data_AQI_Pollutant %>%
  mutate(year = year(Date))

head(AQ_data_AQI_Pollutant)

AQ_data_AQI_Pollutant <- AQ_data_AQI_Pollutant %>%
  select(Date,
         max_AQI,
         max_Pollutant,
         month,
         season,
         year)

head(AQ_data_AQI_Pollutant)

# write_csv(AQ_data_AQI_Pollutant, "final_AQI_2014_2016.csv")
# AQ_data_AQI_Pollutant <- read_csv("final_AQI_2014_2016.csv")

# save data as R object -----------------------------------
save(AQ_data_AQI_Pollutant, file="Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/AQI_final_24h.Rdata")

#####################################################
# reload data ---------------------------------------
# restart R

library(dplyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)

load("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/AQI_final_24h.Rdata")

head(AQ_data_AQI_Pollutant)
str(AQ_data_AQI_Pollutant)


AQ_data_AQI <- AQ_data_AQI_Pollutant %>%
  dplyr::select(Date,
                max_AQI)


########################################################################################
#### plot summary of AQI ---------------------------------------------------------------
########################################################################################

AQ_data_AQI_Pollutant <- AQ_data_AQI_Pollutant %>%
  select(max_AQI,
         max_Pollutant,
         season,
         year)

head(AQ_data_AQI_Pollutant)
str(AQ_data_AQI_Pollutant)

# change name of "PM25" into  PM2.5
AQ_data_AQI_Pollutant$max_Pollutant  <- ifelse(grepl("PM25", AQ_data_AQI_Pollutant$max_Pollutant, ignore.case = TRUE), 
                       "PM2.5", AQ_data_AQI_Pollutant$max_Pollutant)
AQ_data_AQI_Pollutant$max_Pollutant  <- ifelse(grepl("1", AQ_data_AQI_Pollutant$max_Pollutant, ignore.case = TRUE), 
                                               "PM10", AQ_data_AQI_Pollutant$max_Pollutant)
  
  


plot <- ggplot(AQ_data_AQI_Pollutant, aes(max_Pollutant, max_AQI, fill = max_Pollutant)) +
  theme_bw() +
  geom_boxplot() + 
  facet_grid(season ~ .) +
 # guides(fill=FALSE) +   # no legend
  ylim(0, 500) +
  theme( strip.text = element_text(size = 14)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
  ylab(expression("dominat AQI by pollutant")) +
  xlab(expression("dominant pollutant")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=10),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour="black")) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=14, colour="black")) +
  ggtitle("Air Quality Indexes (Umm Al Quwain 2007-2017)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 12, hjust = 0.5)) +


  # add line for the AQI level and the Category
  geom_hline(yintercept =50, col="#00CD00", lty=1, size = 0.8) +
#  geom_text(aes(x = 0.7 , y = 70, label = "Good"), size = 3) +


 geom_hline(yintercept = 100, col="#ffff00", lty=1, size=0.8) +
# geom_text(aes(x = 0.7 , y = 120, label = "Moderate"), size = 3) +

 geom_hline(yintercept = 150, col="#e59400", lty=1, size=0.8) +
# geom_text(aes(x = 1.15 , y = 170, label = "Unhealthy for Sensitive Groups"), size = 3) +

 geom_hline(yintercept = 200, col="#ff0000", lty=1, size=0.8) +
# geom_text(aes(x = 0.7 , y = 220, label = "Unhealthy"), size = 3) +
  
  # remove grids
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # remove background
  
  theme(legend.position="none")
  
plot


#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/"


png(paste0(output_folder,"AQI_season_Umm_Al_Quwain.png"), 
    width = 1000, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()

###################################################################################

#########################################
##### AQI by season and by year #########
#########################################

AQ_data_AQI_Pollutant$year <- as.factor(AQ_data_AQI_Pollutant$year)

plot <- ggplot(AQ_data_AQI_Pollutant, aes(year, max_AQI, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  facet_grid(season ~ .) +
  # guides(fill=FALSE) +   # no legend
  ylim(0, 300) +
  theme( strip.text = element_text(size = 14)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
  ylab(expression("AQI")) +
  xlab(expression(" ")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=10),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour="black")) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=12, colour="black")) +
  ggtitle("Air Quality Indexes (Umm Al Quwain 2007-2017)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 12, hjust = 0.5)) +
  
  
  # add line for the AQI level and the Category
  geom_hline(yintercept =50, col="#00CD00", lty=1, size = 0.6) +
  #  geom_text(aes(x = 0.7 , y = 70, label = "Good"), size = 3) +
  
  
  geom_hline(yintercept = 100, col="#ffff00", lty=1, size=0.6) +
  # geom_text(aes(x = 0.7 , y = 120, label = "Moderate"), size = 3) +
  
  geom_hline(yintercept = 150, col="#e59400", lty=1, size=0.6) +
  # geom_text(aes(x = 1.15 , y = 170, label = "Unhealthy for Sensitive Groups"), size = 3) +
  
  geom_hline(yintercept = 200, col="#ff0000", lty=1, size=0.6) +
  # geom_text(aes(x = 0.7 , y = 220, label = "Unhealthy"), size = 3) +
  
  # remove grids
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # remove background
  
  theme(legend.position="none")

plot


output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/"


png(paste0(output_folder,"AQI_SEASON_Yearly_Umm_Al_Quwain.png"), 
    width = 1500, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


###################################################################################
### Annual AQIs-------------------------------------------------------------------


AQ_data_AQI_Pollutant$year <- as.factor(AQ_data_AQI_Pollutant$year)
str(AQ_data_AQI_Pollutant)
# AQ_data_AQI_Pollutant <- AQ_data_AQI_Pollutant %>%
#   filter(!year == "2013")


plot <- ggplot(AQ_data_AQI_Pollutant, aes(year, max_AQI, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 300) +
  guides(fill=FALSE) +   # no legend
  ylab(expression("Air Quality Index (AQI)")) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=18, colour = "black")) +
  ggtitle("annual distribution of Air Quality Index (Umm Al Quwain)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 16, hjust = 0.5)) +


  # add line for the AQI level and the Category
  geom_hline(yintercept =50, col="#00CD00", lty=1, size = 1) +
#  geom_text(aes(x = 0.7 , y = 54, label = "Good"), size = 8) +
  
  
  geom_hline(yintercept = 100, col="#ffff00", lty=1, size=1) +
#  geom_text(aes(x = 0.7 , y = 104, label = "Moderate"), size = 7)

geom_hline(yintercept = 150, col="#e59400", lty=1, size=1) 
  # geom_text(aes(x = 1.15 , y = 170, label = "Unhealthy for Sensitive Groups"), size = 3) +
  
plot



#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AOD_Umm_Al_Quwain/"


png(paste0(output_folder,"AQI_Yearly_Umm_Al_Quwain.png"), 
    width = 1500, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


##################################################################################
##################################################################################
##################################################################################
##################################################################################
