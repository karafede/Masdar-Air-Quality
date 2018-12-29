library(threadr)
library(readr)
library(dplyr)
library(stringr)
library(rmarkdown)
library(ggplot2)
library(leaflet)
library(htmlwidgets)

# Import daily data for EAD

NCMS_2013 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_NCMS_2013_daily_mean.csv")
NCMS_2014 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_NCMS_2014_daily_mean.csv")
NCMS_2015 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_NCMS_2015_daily_mean.csv")
NCMS_2016 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_NCMS_2016_daily_mean.csv")
NCMS_2017 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_NCMS_2017_daily_mean.csv")

#Import daily filtered data

NCMS_2013_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_NCMS_ 2013 _daily_filtered.csv")
NCMS_2014_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_NCMS_ 2014 _daily_filtered.csv")
NCMS_2015_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_NCMS_ 2015 _daily_filtered.csv")
NCMS_2016_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_NCMS_ 2016 _daily_filtered.csv")
NCMS_2017_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_NCMS_ 2017 _daily_filtered.csv")

# bind data together
NCMS <- rbind(NCMS_2013 ,NCMS_2014, NCMS_2015, NCMS_2016, NCMS_2017)
NCMS_filtered <-rbind(NCMS_2013_filtered, NCMS_2014_filtered,NCMS_2015_filtered, NCMS_2016_filtered, NCMS_2017_filtered)

#Define get_sites function

get_sites <- function(var) {
  NCMS_PM25 <- NCMS %>%
    filter(Pollutant == var) %>%
    distinct(Site, Latitude, Longitude)
NCMS_PM25
}
  
#Define get_measurement_time_series function 

get_measurement_time_series <- function(station, pollutant) {
  NCMS <- NCMS %>%
     mutate(date = Date) %>%
     dplyr:: select(date,
            Site,
            Pollutant,
            Daily_mean) %>%
    filter(Site == station)
  NCMS_filtered <- NCMS_filtered %>%
    mutate(date = Date) %>%
    dplyr:: select(date,
                   Site,
                   Pollutant,
                   Daily_mean) %>%
    filter(Site == station)
  data_time <- NCMS %>%
    spread(Pollutant, Daily_mean)
  data_time_filtered <- NCMS_filtered %>%
    spread(Pollutant, Daily_mean)
  time_series <- data_frame_to_timeseries(data_time)
  time_series_filtered <- data_frame_to_timeseries(data_time_filtered)
  All_data <- cbind(time_series,time_series_filtered)
  All_data
}

#Define interactive_plot function

interactive_plot <- function(ts, station, group, pollu) {
  check_<-row.names(ts)
  
  if (!is.null(ts) & is.element(pollu, check_) ) {
    colour_vector <- threadr::ggplot2_colours(45)
    # NO2
    if (pollutant == "NO<sub>2</sub>") {
      
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily NO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>%
        dyRangeSelector()
    }
    #SO2
    if (pollutant == "SO<sub>2</sub>") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily SO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    }
    #CO
    if (pollutant == "CO") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily CO (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    }
    #O3
    if (pollutant == "O<sub>3</sub>") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily O<sub>3</sub> (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    }
    #PM10
    if (pollutant == "PM<sub>10</sub>") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    }
    #PM2.5
    if (pollutant == "PM<sub>2.5</sub>") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily PM<sub>2.5</sub> (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    }
    #Lower Ambient Temperature
    if (pollutant == "LowerAmbientTemperature") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily Temp. (<sup>o</sup>C)") %>% 
        dyRangeSelector()
    }
    #Upper Ambient Temperature
    if (pollutant == "UpperAmbientTemperature") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily Temp. (<sup>o</sup>C)") %>% 
        dyRangeSelector()
    }
    #Barometric Pressure
    if (pollutant == "BarometricPressure") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily Atmo. Pressure (Pa)") %>% 
        dyRangeSelector()
    }
    #RelativeHumidity
    if (pollutant == "RelativeHumidity") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily Humiduty (%)") %>% 
        dyRangeSelector()
    }
    #Wind Direction
    if (pollutant == "WindDirection") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily (degree)") %>% 
        dyRangeSelector()
    }
    #Wind Speed 
    if (pollutant == "WindSpeed") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily Speed (km hr<sup>-1</sup>)") %>% 
        dyRangeSelector()
    }
    #H2S
    if (pollutant == "H<sub>2</sub>S") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily H<sub>2</sub>S (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #Toluene
    if (pollutant == "Toluene") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily Toluene (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #O_Xylene
    if (pollutant == "O_Xylene") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily O_Xylene (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #EthylBenzene
    if (pollutant == "EthylBenzene") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily EthylBenzene (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #mp_xylene_
    if (pollutant == "mp_xylene_") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily mp_xylene (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #Benzene
    if (pollutant == "Benzene") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily Benzene (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
    } 
    #Noise
    if (pollutant == "Noise") {
      data_both<- ts[pollu,]
      data_both_ts<-as.ts(data_both[1])
      data_both_ts_2<-as.ts(data_both[2])
      ts_xxx <- cbind(data_both_ts$time_series, data_both_ts_2$time_series_filtered)
      plot <- dygraph(ts_xxx, group = group, main = paste(station, " - ", pollutant)) %>% 
        dySeries("..1",label = "with outliers", color = "red") %>%
        dySeries("..2",label = "no outliers", color = "blue") %>%
        dyAxis("y", label = "Daily Noise (Dba)") %>% 
        dyRangeSelector()
    } 
    plot
  }
}

#Define interactive_map_index function

interactive_map_index <- function(df) {
  map <- leaflet() %>%
    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Map data &copy;2018 Google', group = "GoogleMap") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Esri.WorldImagery", group = "Images") %>%
    addMarkers(data = df, lng = ~ Longitude, lat = ~ Latitude,
               popup = ~ Site, group = "Sites") %>%
    addPolygons(stroke = TRUE, data = shp_UAE,
                weight = 1.5, color = ~ colorNumeric(c("#a56e6e", "#7a7acc", "#FFFF00", "#ff0000", "#be68be", "#7fbf7f", "#008000", "#0000ff"), shp_UAE$ID_1)(ID_1),
                fillOpacity = 0.5,
                group = "shape_UAE") %>%
    addLayersControl(baseGroups = c("GoogleMap", "Toner", "Images"),
                     overlayGroups = c("Sites"))
  map
}

#Define interactive_map function

interactive_map <- function(df) {
  map <- leaflet() %>%
    setView(lng = 55.9971, lat = 25.3302, zoom = 9) %>%  
    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Map data &copy;2018 Google', group = "GoogleMap") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Esri.WorldImagery", group = "Images") %>%
    addMarkers(data = df, lng = ~ Longitude, lat = ~ Latitude,
               popup = ~ Site, group = "Sites") %>%
    addLayersControl(baseGroups = c("GoogleMap", "Toner", "Images"),
                     overlayGroups = c("Sites"))
  map
}