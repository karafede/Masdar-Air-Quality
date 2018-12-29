library(readr)
library(ggplot2)
library(threadr)
library(dplyr)
library(stringr)
library(rmarkdown)
library(leaflet)
library(htmlwidgets)

# Import daily data for DM

DM_2013 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_DM_2013_daily_mean.csv")
DM_2014 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_DM_2014_daily_mean.csv")
DM_2015 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_DM_2015_daily_mean.csv")
DM_2016 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_DM_2016_daily_mean.csv")
DM_2017 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/database_DM_2017_daily_mean.csv")

# Import daily filtered data

DM_2013_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_DM_ 2013 _daily_filtered.csv")
DM_2014_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_DM_ 2014 _daily_filtered.csv")
DM_2015_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_DM_ 2015 _daily_filtered.csv")
DM_2016_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_DM_ 2016 _daily_filtered.csv")
DM_2017_filtered <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean/Daily_Filtered_4_Box/database_DM_ 2017 _daily_filtered.csv")

 # bind data together

 DM <- rbind(DM_2013, DM_2014, DM_2015, DM_2016, DM_2017)
 DM_filtered <-rbind(DM_2013_filtered, DM_2014_filtered, DM_2015_filtered, DM_2016_filtered, DM_2017_filtered)

#Define get_sites function
 
 get_sites <- function(var) {
   DM_PM25 <- DM %>%
     filter(Pollutant == var) %>%
     distinct(Site, Latitude, Longitude)
   DM_PM25
 }
 
#Define get_measurements_time_series function
 
get_measurement_time_series <- function(station, pollutant) {
    DM <- DM %>%
      mutate(date = Date) %>%
     dplyr:: select(date,
            Site,
            Pollutant,
            Daily_mean) %>%
    filter(Site == station)
    DM_filtered <- DM_filtered %>%
      mutate(date = Date) %>%
      dplyr:: select(date,
                     Site,
                     Pollutant,
                     Daily_mean) %>%
      filter(Site == station)
data_time <- DM %>%
  spread(Pollutant, Daily_mean)
data_time_filtered <- DM_filtered %>%
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

#Define interactive_map funcion

interactive_map <- function(df) {
  map <- leaflet() %>%
    setView(lng = 55.3661995, lat = 25.0268002, zoom = 9) %>% 
    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Map data &copy;2018 Google', group = "GoogleMap") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Esri.WorldImagery", group = "Images") %>%
    addMarkers(data = df, lng = ~ Longitude, lat = ~ Latitude,
               popup = ~ Site, group = "Sites") %>%
    addLayersControl(baseGroups = c("GoogleMap", "Toner", "Images"),
                     overlayGroups = c("Sites"))
  map
}