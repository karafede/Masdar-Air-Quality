library(dplyr)
library(readr)
library(lubridate)
library(dygraphs)
library(tidyr)
library(ggplot2)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/Scripts")
source("hourly2daily.r")
setwd('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data')

#FEED JUST ONE STATION AT THE TIME

#NCMS_2013 <- read_csv('database_NCMS data 2013_hourly.csv')
#NCMS_2014 <- read_csv('database_NCMS data 2014_hourly.csv')
#NCMS_2015 <- read_csv('database_NCMS data 2015_hourly.csv')
#NCMS_2016 <- read_csv('database_NCMS data 2016_hourly.csv')
#NCMS_2017 <- read_csv('database_NCMS data 2017_hourly.csv')

#DM_2013 <- read_csv('database_DM data 2013_hourly.csv')
#DM_2014 <- read_csv('database_DM data 2014_hourly.csv')
#DM_2015 <- read_csv('database_DM data 2015_hourly.csv')
#DM_2016 <- read_csv('database_DM data 2016_hourly.csv')
#DM_2017 <- read_csv('database_DM data 2017_hourly.csv')

EAD_2013 <- read_csv('database_EAD data 2013_hourly.csv')
EAD_2014 <- read_csv('database_EAD data 2014_hourly.csv')
EAD_2015 <- read_csv('database_EAD data 2015_hourly.csv')
EAD_2016 <- read_csv('database_EAD data 2016_hourly.csv')
EAD_2017 <- read_csv('database_EAD data 2017_hourly.csv')

# ENABLE list_year ACCORDING TO FED STATION
# list_year<- list(NCMS_2013,NCMS_2014,NCMS_2015,NCMS_2016,NCMS_2017)
# list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016,DM_2017)
list_year<- list(EAD_2013,EAD_2014,EAD_2015,EAD_2016,EAD_2017)

xx=0
for (aa in list_year){
  DM<- as.data.frame(aa)
#  DM[,6][DM[, 6] < 0] <- NA # this is to replace the negative values with NA for all the pollutants
  DM[,10][DM[, 10] < 0] <- NA # this is to replace the negative values with NA for all the pollutants
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
  names(daily_data_all)[names(daily_data_all) == 'data_capture$Data_Capture'] <- 'data_capture'

#SELECT ONLY PATH MATCHING WITH FED STATION
  
#  file_name_data <- paste("Daily_Mean/database_NCMS_", year_s,"_daily_mean.csv",sep = "")
#  file_name_data <- paste("Daily_Mean/database_DM_", year_s,"_daily_mean.csv",sep = "")
  file_name_data <- paste("Daily_Mean/database_EAD_", year_s,"_daily_mean.csv",sep = "")
  write.csv(daily_data_all, file = file_name_data)
  xx=xx+1
}