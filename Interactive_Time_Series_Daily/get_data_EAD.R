library(threadr)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(rmarkdown)
library(leaflet)
library(htmlwidgets)

# Import daily data for EAD

 EAD_2013 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_EAD_2013_daily_mean.csv")
 EAD_2014 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_EAD_2014_daily_mean.csv")
 EAD_2015 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_EAD_2015_daily_mean.csv")
 EAD_2016 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_EAD_2016_daily_mean.csv")
 EAD_2017 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_EAD_2017_daily_mean.csv")
 
# Import daily filtered data
 
 EAD_2013_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_EAD_ 2013 _daily_filtered.csv")
 EAD_2014_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_EAD_ 2014 _daily_filtered.csv")
 EAD_2015_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_EAD_ 2015 _daily_filtered.csv")
 EAD_2016_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_EAD_ 2016 _daily_filtered.csv")
 EAD_2017_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_EAD_ 2017 _daily_filtered.csv")
 
 # bind data together
 
 EAD <- rbind(EAD_2013, EAD_2014, EAD_2015, EAD_2016, EAD_2017)
 EAD_filtered <-rbind(EAD_2013_filtered, EAD_2014_filtered, EAD_2015_filtered, EAD_2016_filtered,EAD_2017_filtered)

 #Define get_sites function
 
 get_sites <- function(var) {
   EAD_PM25 <- EAD %>%
     filter(Pollutant == var) %>%
     distinct(Site, Latitude, Longitude)
   EAD_PM25
 }
 
#Define get_measurement_time_series funcion
  
get_measurement_time_series <- function(station, pollutant) {
    EAD <- EAD %>%
      mutate(date = Date) %>%
     dplyr:: select(date,
            Site,
            Pollutant,
            Daily_mean) %>%
    filter(Site == station)
    EAD_filtered <- EAD_filtered %>%
      mutate(date = Date) %>%
      dplyr:: select(date,
                     Site,
                     Pollutant,
                     Daily_mean) %>%
      filter(Site == station)
data_time <- EAD %>%
  spread(Pollutant, Daily_mean)
data_time_filtered <- EAD_filtered %>%
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
    #Relative Humidity
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

#Define interactive_map function

interactive_map <- function(df) {
  map <- leaflet() %>%
    setView(lng = 55.0052, lat = 24.1739, zoom = 8) %>% 
    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Map data &copy;2018 Google', group = "GoogleMap") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Esri.WorldImagery", group = "Images") %>%
    addMarkers(data = df, lng = ~ Longitude, lat = ~ Latitude,
               popup = ~ Site, group = "Sites") %>%
    addLayersControl(baseGroups = c("GoogleMap", "Toner", "Images"),
                     overlayGroups = c("Sites"))
  map
}