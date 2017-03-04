library(dplyr)
library(readr)
library(threadr)
library(dygraphs)
library(tidyr)
library(ggplot2)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files")
source("hourly2daily.r")

# FEED ONLY ONE YEAR HOURLY DATA OF STATIONS FROM THE SAME AUTHORITY for example 2013 data from the Dubai Municipality (DM)
DM_2013 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_NCMS_2013_hourly.csv')
DM_2014 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_NCMS_2014_hourly.csv')
DM_2015 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_NCMS_2015_hourly.csv')
DM_2016 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_NCMS_2016_hourly.csv')
#DM_2014 <- read.csv('D:/database_DM_2014_hourly.csv')
#DM_2015 <- read.csv('D:/database_DM_2015_hourly.csv')
#DM_2016 <- read.csv('D:/database_DM_2016_hourly.csv')

# DM_month <-subset(DM_2013, DateTime > '2013-09-01 00:00:00 UTC' & DateTime < '2013-12-31 00:00:00 UTC')

# 2.5 range 
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016)
xx=0
for (aa in list_year){
  DM<- as.data.frame(aa)
  DM[,6][DM[, 6] < 0] <- NA # this is to replace the negative values with NA for all the pollutants
  DM$DateTime<- DM$DateTime + 300
  
  
  #DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
  #DM<-DM_2013
  station <- unique(DM$Site)
  daily_data_all<-data.frame()
  for (i in station){
    
    stat<- filter(DM, Site == i )
    pollu <- unique(stat$Pollutant)
    
    for(j in pollu){
      
      to_be_converted <- filter(stat, Pollutant == j )
      output_Daily_fun <- hour2day(to_be_converted)
      daily_data_all<-rbind(daily_data_all,output_Daily_fun)
    }
  }
  year_s<-2013 + xx
  file_name_data <- paste("Daily_mean/database_NCMS_", year_s,"_daily_mean.csv",sep = "")
  write.csv(daily_data_all, file = file_name_data)
  xx=xx+1
}
