
# time series with hourly UAE pollution data

library(readr)
library(threadr)
library(dygraphs)
library(tidyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV")


NCMS_new <- read_csv("database_EAD_2016_hourly.csv")
# replace NaN (not a Number with NA that is a missing value)
NCMS_new[sapply(NCMS_new,is.na)] = NA 

# NCMS$DateTime <- as.POSIXct(as.Date(NCMS$DateTime,origin="1899-12-30"), tz="UTC") - 3*60*60
# str(NCMS)

NCMS_new$DateTime <- as.POSIXct(as.Date(NCMS_new$DateTime,origin="1899-12-30"), tz="UTC") 


write_csv(NCMS_new, "Arranged dates/database_EAD_2016_hourly.csv" )



# the files processed by my Machine

NCMS<- read_csv("database_EAD_2015_hourly.csv")
# replace NaN (not a Number with NA that is a missing value)
NCMS[sapply(NCMS,is.na)] = NA 

# NCMS$DateTime <- as.POSIXct(as.Date(NCMS$DateTime,origin="1899-12-30"), tz="UTC") - 3*60*60
# str(NCMS)


NCMS <- NCMS %>%
  mutate(date = mdy_hms(DateTime, tz = "UTC")) %>%
  mutate(DateTime = date) %>%
  select(-date)

write_csv(NCMS, "Arranged dates/database_EAD_2015_hourly.csv" )
  
  