
library(RCurl)
library(stringr)
library(plyr)
library(dplyr)
library(threadr)
library(gdalUtils)
library(rgdal)
library(raster)
library(RNetCDF)
library(readr)
library(gstat)
library(curl)
library(leaflet)
library(webshot)
library(htmlwidgets)
library(sp)
library(tools)

library(aws.s3)

# if you want to download data directlry from AMAZON DB
# https://console.aws.amazon.com/athena/home?region=us-east-1

##################################################
### run this in the Athena AWS ##################

# UAE
# run the following queries from Athena (the openaq library should be saved in the system first)
# download the results in .csv format

# SELECT *
#   FROM openaq
# WHERE country='AE'


# Baharein
# SELECT *
#   FROM openaq
# WHERE country='BH'


# KUWAIT
# SELECT *
#   FROM openaq
# WHERE country='KW'


# options(warn=-1)

# download data from AMAZON
# https://gist.github.com/jflasher/573525aff9a5d8a966e5718272ceb25a



setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/OpenAQ_data")
# setwd("/disk3/fkaragulian/MODIS_AOD/2017/001/")
# folder_day <- as.character("001")

wd <- getwd()
Sys.time()

current_date <- str_sub(Sys.time(), start = 1, end = -10)
str(current_date)
folder_year <- str_sub(current_date, start = 1, end = -7)

file = paste0("https://openaq-data.s3.amazonaws.com/2018-03-02.csv") 
# mapply(download.file, file,basename(file)) 

###########################
# country availability ####
###########################

library("ropenaq")
countries_table <- aq_countries()
library("knitr")
write.csv(countries_table, "country_table.csv")

# info source from city ####

cities_table <- aq_cities()
kable(head(cities_table))

cities_tableIndia <- aq_cities(country="IN", limit = 10, page = 1)
cities_tableUAE <- aq_cities(country="AE", limit = 10, page = 1)
cities_table_VIETNAM <- aq_cities(country="VN", limit = 10, page = 1)


# location - coordinates 
locations_chennai <- aq_locations(country = "IN", city = "Chennai", parameter = "pm25")
locations_Mumbai <- aq_locations(country = "IN", city = "Mumbai", parameter = "pm25")
locations_Hanoi <- aq_locations(country = "VN", city = "Hanoi", parameter = "pm25")



locations_Abu_Dhabi <- aq_locations(country = "AE", city = "Abu+Dhabi", parameter = "pm25")
locations_Delhi <- aq_locations(country = "IN", city = "Delhi", parameter = "pm25")


# getting measuremtnes
results_table <- aq_measurements(country = "IN", city = "Delhi", parameter = "pm25", limit = 10, page = 1)
results_table <- aq_measurements(country = "VN", city = "Hanoi", parameter = "pm25", limit = 1000, page = 1)


# kable(results_table)


# latest measurments
tableLatest <- aq_latest(country="IN", city="Delhi")
tableLatest <- aq_latest(country="AE", city="Abu+Dhabi")
results_table <- aq_measurements(country = "AE", city = "Abu+Dhabi", parameter = "pm25", limit = 1000, page = 1)

results_table <- aq_measurements(country = "IN", city = "Mumbai", parameter = "pm25", 
                                 date_from = "2018-02-02", date_to = "2018-02-05", limit = 1000, page = 1)

results_table <- aq_measurements(country = "IN", city = "Delhi", parameter = "pm25", 
                                 date_from = "2018-02-02", date_to = "2018-02-05", limit = 1000, page = 1)


##################################################
### run this in the Athena AWS ##################

# UAE

# SELECT *
#   FROM openaq
# WHERE country='AE'


# Baharein 
# SELECT *
#   FROM openaq
# WHERE country='BH'


# KUWAIT
# SELECT *
#   FROM openaq
# WHERE country='KW'


### Map of OpenAQ locations

library("ggplot2")
library("ropenaq")
library("dplyr")

dataGeo <- aq_locations()
dataGeo <- filter(dataGeo, location != "Test Prueba", location != "PA")


library("rworldmap")

worldMap <- map_data(map="world")

gg <- ggplot() + geom_map(data=worldMap, map=worldMap,
                          aes(map_id=region, x=long, y=lat),
                          fill = "grey60")
gg


plotMap <- gg +
  geom_point(data = dataGeo, aes(x=longitude, y=latitude), size=1, col = "#EE9F8E")+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle("OpenAQ data sources with geographical coordinates") +
  theme(plot.title = element_text(lineheight=1, face="bold"))
print(plotMap)

####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

