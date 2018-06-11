library(threadr)
library(dplyr)
library(leaflet)
library(readr)
library(lubridate)
library(rowr)

# load data

# Ozone data (8-hr)--------------------------------------------------------------------
EAD_O3_2013 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_EAD_2013_O3_daily.csv")
EAD_O3_2014 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_EAD_2014_O3_daily.csv")
EAD_O3_2015 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_EAD_2015_O3_daily.csv")
EAD_O3_2016 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_EAD_2016_O3_daily.csv")
EAD_O3_2017 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_EAD_2017_O3_daily.csv")

DM_O3_2013 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_DM_2013_O3_daily.csv")
DM_O3_2014 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_DM_2014_O3_daily.csv")
DM_O3_2015 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_DM_2015_O3_daily.csv")
DM_O3_2016 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_DM_2016_O3_daily.csv")
DM_O3_2017 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_DM_2017_O3_daily.csv")

NCMS_O3_2013 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_NCMS_2013_O3_daily.csv")
NCMS_O3_2014 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_NCMS_2014_O3_daily.csv")
NCMS_O3_2015 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_NCMS_2015_O3_daily.csv")
NCMS_O3_2016 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_NCMS_2016_O3_daily.csv")
NCMS_O3_2017 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/database_NCMS_2017_O3_daily.csv")

# bind data together
UAE_O3 <- rbind(EAD_O3_2013, EAD_O3_2014, EAD_O3_2015, EAD_O3_2016, EAD_O3_2017,
                DM_O3_2013, DM_O3_2014, DM_O3_2015, DM_O3_2016, DM_O3_2017,
                NCMS_O3_2013, NCMS_O3_2014, NCMS_O3_2015, NCMS_O3_2016, NCMS_O3_2017)

# remove lines wtih NA in the Mean_8hour column
 UAE_O3 <- UAE_O3[!is.na(UAE_O3$Mean_8hour),]

# UAE_O3 <- UAE_O3 %>%
#   mutate(DateTime = ymd_hms(Date))

# # add date field only
# UAE_O3 <- UAE_O3 %>%
#   mutate(Date = date(DateTime))

# conversion from ug/m3 to ppb (WHO conversion factor)
 UAE_O3$MAX_8hour <- (UAE_O3$MAX_8hour) /1.96 

UAE_O3$Site <- as.character(UAE_O3$Site)

str(UAE_O3)

EAD <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/Info_Stations/Stations_EAD_info_2.csv")
DM <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/Info_Stations/Stations_DM_info_2.csv")
NCMS <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/Info_Stations/Stations_NCMS_info_2.csv")

station_info <- rbind(EAD, DM, NCMS)

UAE_O3$Pollutant <- "O3"

# attach infos to ozone data
UAE_O3 <- UAE_O3 %>%
  left_join(station_info, c("Site", "Pollutant"))


names(UAE_O3)[names(UAE_O3) == 'Site Type'] <- 'Site_Type'
# names(UAE_O3)[names(UAE_O3) == 'Value'] <- 'Max_O3_8hr'
UAE_O3$Site <- as.character(UAE_O3$Site)
names(UAE_O3)[names(UAE_O3) == 'Pollutant'] <- 'Pollutant_O3'
names(UAE_O3)[names(UAE_O3) == 'MAX_8hour'] <- 'Max_O3_8hr'


# str(UAE_O3)

UAE_O3 <- UAE_O3 %>%
  select(Date,
         Site,
         Pollutant_O3,
         Site_Type,
         Latitude,
         Longitude,
         Max_O3_8hr)

# create a field for the date & hour
UAE_O3 <- UAE_O3 %>%
  mutate(Date = date(Date))

# write_csv(UAE_O3, "D:/AQI/UAE_O3_8h_Max.csv")


# CO data (8-hr)--------------------------------------------------------------------

EAD_CO_2013 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_EAD_2013_CO_daily.csv")
EAD_CO_2014 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_EAD_2014_CO_daily.csv")
EAD_CO_2015 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_EAD_2015_CO_daily.csv")
EAD_CO_2016 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_EAD_2016_CO_daily.csv")
EAD_CO_2017 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_EAD_2017_CO_daily.csv")

DM_CO_2013 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_DM_2013_CO_daily.csv")
DM_CO_2014 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_DM_2014_CO_daily.csv")
DM_CO_2015 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_DM_2015_CO_daily.csv")
DM_CO_2016 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_DM_2016_CO_daily.csv")
DM_CO_2017 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_DM_2017_CO_daily.csv")

NCMS_CO_2013 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_NCMS_2013_CO_daily.csv")
NCMS_CO_2014 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_NCMS_2014_CO_daily.csv")
NCMS_CO_2015 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_NCMS_2015_CO_daily.csv")
NCMS_CO_2016 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_NCMS_2016_CO_daily.csv")
NCMS_CO_2017 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO/database_NCMS_2017_CO_daily.csv")

# bind data together
UAE_CO <- rbind(EAD_CO_2013, EAD_CO_2014, EAD_CO_2015, EAD_CO_2016, EAD_CO_2017,
                DM_CO_2013, DM_CO_2014, DM_CO_2015, DM_CO_2016, DM_CO_2017,
                NCMS_CO_2013, NCMS_CO_2014, NCMS_CO_2015, NCMS_CO_2016, NCMS_CO_2017)

# UAE_CO <- UAE_CO %>%
#   mutate(DateTime = ymd_hms(Date))


# remove lines wtih NA in the Mean_8hour column
UAE_CO <- UAE_CO[!is.na(UAE_CO$Mean_8hour),]


# conversion from mg/m3 to ppm (WHO conversion factor)
UAE_CO$MAX_8hour <- (UAE_CO$MAX_8hour) /1.15 


UAE_CO$Site <- as.character(UAE_CO$Site)
str(UAE_CO)

# station_info <- station_info %>%
#   filter(Pollutant =="CO")

# attach infos to ozone data
UAE_CO <- UAE_CO %>%
  left_join(station_info, c("Site", "Pollutant"))


names(UAE_CO)[names(UAE_CO) == 'Site Type'] <- 'Site_Type'
# names(UAE_O3)[names(UAE_O3) == 'Value'] <- 'Max_O3_8hr'
UAE_CO$Site <- as.character(UAE_CO$Site)
names(UAE_CO)[names(UAE_CO) == 'Pollutant'] <- 'Pollutant_CO'
names(UAE_CO)[names(UAE_CO) == 'MAX_8hour'] <- 'Max_CO_8hr'



UAE_CO <- UAE_CO %>%
  select(Date,
         Site,
         Pollutant_CO,
         Site_Type,
         Latitude,
         Longitude,
         Max_CO_8hr)


# create a field for the date & hour
UAE_CO <- UAE_CO %>%
  mutate(Date = date(Date))

# write_csv(UAE_CO, "D:/AQI/UAE_CO_8h_Max.csv")

# remove stations with outliers
UAE_CO <- UAE_CO %>%
  filter(!Site == "Al Ruwais") %>%
  filter(!Site == "Al Tawia") %>%
  filter(!Site == "Bain Aljesrain") %>%
  filter(!Site == "Jebel Ali Village") %>%
  filter(!Site == "Karama") %>%
  filter(!Site == "Safa") %>%
  filter(!Site == "Zabeel")



##################################################################################
# AQ data (24-hr daily averages, filtered from outliers 4-box plots)-------------


EAD_AQ_2013 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_EAD_ 2013 _daily_filtered.csv")
EAD_AQ_2014 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_EAD_ 2014 _daily_filtered.csv")
EAD_AQ_2015 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_EAD_ 2015 _daily_filtered.csv")
EAD_AQ_2016 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_EAD_ 2016 _daily_filtered.csv")
EAD_AQ_2017 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_EAD_ 2017 _daily_filtered.csv")

DM_AQ_2013 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_DM_ 2013 _daily_filtered.csv")
DM_AQ_2014 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_DM_ 2014 _daily_filtered.csv")
DM_AQ_2015 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_DM_ 2015 _daily_filtered.csv")
DM_AQ_2016 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_DM_ 2016 _daily_filtered.csv")
DM_AQ_2017 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_DM_ 2017 _daily_filtered.csv")

NCMS_AQ_2013 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_NCMS_ 2013 _daily_filtered.csv")
NCMS_AQ_2014 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_NCMS_ 2014 _daily_filtered.csv")
NCMS_AQ_2015 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_NCMS_ 2015 _daily_filtered.csv")
NCMS_AQ_2016 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_NCMS_ 2016 _daily_filtered.csv")
NCMS_AQ_2017 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_NCMS_ 2017 _daily_filtered.csv")

# bind data together
UAE_AQ <- rbind(EAD_AQ_2013, EAD_AQ_2014, EAD_AQ_2015, EAD_AQ_2016, EAD_AQ_2017,
                DM_AQ_2013, DM_AQ_2014, DM_AQ_2015, DM_AQ_2016, DM_AQ_2017,
                NCMS_AQ_2013, NCMS_AQ_2014, NCMS_AQ_2015, NCMS_AQ_2016, NCMS_AQ_2017)

UAE_AQ$Site <- as.character(UAE_AQ$Site)
UAE_AQ$Site_Type <- as.character(UAE_AQ$Site_Type)
UAE_AQ$Pollutant <- as.character(UAE_AQ$Pollutant)
 str(UAE_AQ)

UAE_AQ$Date <- as.Date(UAE_AQ$Date)
str(UAE_AQ)

UAE_AQ <- UAE_AQ %>%
  select(Date,
         Site,
         Pollutant,
         Site_Type,
         Latitude,
         Longitude,
         Daily_mean)

str(UAE_AQ)

# select only PM25
UAE_PM25 <- UAE_AQ %>%
  filter(Pollutant == c("PM2.5"))

names(UAE_PM25)[names(UAE_PM25) == 'Pollutant'] <- 'Pollutant_PM25'

str(UAE_PM25)

# select only PM10
UAE_PM10 <- UAE_AQ %>%
  filter(Pollutant == c("PM10"))

names(UAE_PM10)[names(UAE_PM10) == 'Pollutant'] <- 'Pollutant_PM10'

names(UAE_PM25)[names(UAE_PM25) == 'Daily_mean'] <- 'PM25_24hr'
names(UAE_PM10)[names(UAE_PM10) == 'Daily_mean'] <- 'PM10_24hr'


str(UAE_PM25)


# select only NO2
UAE_NO2 <- UAE_AQ %>%
  filter(Pollutant == c("NO2"))

names(UAE_NO2)[names(UAE_NO2) == 'Pollutant'] <- 'Pollutant_NO2'

str(UAE_NO2)


# select only SO2
UAE_SO2 <- UAE_AQ %>%
  filter(Pollutant == c("SO2"))

names(UAE_SO2)[names(UAE_SO2) == 'Pollutant'] <- 'Pollutant_SO2'

str(UAE_SO2)

# SO2 
UAE_SO2$Daily_mean <- UAE_SO2$Daily_mean /2.62

# NO2 
UAE_NO2$Daily_mean <- UAE_NO2$Daily_mean /1.88

names(UAE_SO2)[names(UAE_SO2) == 'Daily_mean'] <- 'SO2_24hr'
names(UAE_NO2)[names(UAE_NO2) == 'Daily_mean'] <- 'NO2_24hr'

ZZZ <- UAE_NO2 %>%
  left_join(UAE_SO2, c("Date",   "Site","Site_Type", "Latitude", "Longitude"))


BBB <- ZZZ %>%
  left_join(UAE_O3, by = c("Date", "Site", "Site_Type", "Latitude", "Longitude"))

CCC <- BBB %>%
  left_join(UAE_CO, by = c("Date", "Site"))


UAE_PM <- merge(UAE_PM10, UAE_PM25)


# join PM2.5 and PM10 #########################

AQ_data <- CCC %>%
  left_join(UAE_PM, by = c("Date", "Site", "Latitude", "Longitude"))

head(AQ_data)

AQ_data_clean <- AQ_data %>%
  select(- Site_Type.x,
         - Site_Type.y)
AQ_data_clean[,c("Pollutant_NO2","Longitude")]<-AQ_data_clean[,c("Longitude","Pollutant_NO2")]
names(AQ_data_clean)[names(AQ_data_clean) == 'Longitude'] <- 'Pollutant_NO2_2'
names(AQ_data_clean)[names(AQ_data_clean) == 'Pollutant_NO2'] <- 'Longitude'
names(AQ_data_clean)[names(AQ_data_clean) == 'Pollutant_NO2_2'] <- 'Pollutant_NO2'


head(AQ_data_clean)

# make some cross checks 
ab<- filter( AQ_data, AQ_data$Site == NA)
ab<- filter( AQ_data, AQ_data$Latitude ==  NA)
ab<- filter( AQ_data, AQ_data$Longitude ==  NA)
ab<- filter( AQ_data, AQ_data$Site ==  "Hamdan Street")


 save(AQ_data_clean, file="Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Air_Quality_Index/AQ_data_all_clean_24h_new.Rdata")
 
 
 
############################################################################# 
########## END ##############################################################
#############################################################################
#############################################################################
