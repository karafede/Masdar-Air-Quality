
library(dplyr)
library(readr)
library(threadr)
library(dygraphs)
library(tidyr)
library(ggplot2)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files")
source("new_boxplot_fun.r")

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

#DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
#DM<-DM_2013
raw_sel <- subset(DM, select=c("DateTime", "Site","Pollutant","Value"))
station <- unique(raw_sel$Site)
david<- vector()
number_outliers<-data.frame()
for (i in station){
  # i<-"Deira" # to be deleted
  stat<- filter(raw_sel, Site == i )
  pollu <- unique(stat$Pollutant)
  #number_out<-data.frame()
  
  for(j in pollu){
    #  j<-"PM2.5"# to be deleted
    to_walash <- filter(stat, Pollutant == j )
    
    if (is.null(to_walash)){
    } else {
      output_OL <- walash_outlier(to_walash, 2.5) # walash function which have two outcomes as a list
      output_error<- as.numeric(output_OL[2])
      if (output_error==1 | output_error==2){      # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
        # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
        output_OL_data<- as.data.frame(output_OL[1])
        dave <- (DM$Site==i & DM$Pollutant == j )
        ind_true <- which(dave)
        # q=1 k<-ind_true[1] l=2
        DM$Value[ind_true]<- output_OL_data$Value
        
        # the outliers
        outlier_num<-as.numeric(output_OL[3])
        number_out<- data.frame(Site=i, Pollutant=j, Number_Outliers=outlier_num) 
        number_outliers<-rbind(number_outliers,number_out)
        remove(number_out)
        
      }
      if (output_error==0) {
        outlier_num<-as.numeric(output_OL[3])
        number_out<- data.frame(Site=i, Pollutant=j, Number_Outliers=outlier_num) 
        number_outliers<-rbind(number_outliers,number_out)
        remove(number_out)
      }
      
    }
    
  }
}

year_s<-2013 + xx
file_name_out <- paste("filtered_2_5_box/database_NCMS_", year_s, "_outliers.csv")
file_name_data <- paste("filtered_2_5_box/database_NCMS_", year_s, "_hourly_filtered.csv")
write.csv(number_outliers, file = file_name_out)
write.csv(DM, file = file_name_data)
xx=xx+1
}

# with 3 range
remove(DM)
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016)
xx=0
for (aa in list_year){
  DM<- as.data.frame(aa)
  
  #DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
  #DM<-DM_2013
  raw_sel <- subset(DM, select=c("DateTime", "Site","Pollutant","Value"))
  station <- unique(raw_sel$Site)
  david<- vector()
  number_outliers<-data.frame()
  for (i in station){
    # i<-"Deira" # to be deleted
    stat<- filter(raw_sel, Site == i )
    pollu <- unique(stat$Pollutant)
    #number_out<-data.frame()
    
    for(j in pollu){
      #  j<-"PM2.5"# to be deleted
      to_walash <- filter(stat, Pollutant == j )
      
      if (is.null(to_walash)){
      } else {
        output_OL <- walash_outlier(to_walash, 3) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        if (output_error==1 | output_error==2){      # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          # q=1 k<-ind_true[1] l=2
          DM$Value[ind_true]<- output_OL_data$Value
          
          # the outliers
          outlier_num<-as.numeric(output_OL[3])
          number_out<- data.frame(Site=i, Pollutant=j, Number_Outliers=outlier_num) 
          number_outliers<-rbind(number_outliers,number_out)
          remove(number_out)
          
        }
        if (output_error==0) {
          outlier_num<-as.numeric(output_OL[3])
          number_out<- data.frame(Site=i, Pollutant=j, Number_Outliers=outlier_num) 
          number_outliers<-rbind(number_outliers,number_out)
          remove(number_out)
        }
        
      }
      
    }
  }
  
  year_s<-2013 + xx
  file_name_out <- paste("filtered_3_box/database_NCMS_", year_s, "_outliers.csv")
  file_name_data <- paste("filtered_3_box/database_NCMS_", year_s, "_hourly_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}



# end
#############################################################################
##############################################################################

# dawit_DG<-output_OL[7]


DM_test <- DM %>%
  filter(Site == "Deira") %>%
  filter(Pollutant == "PM2.5")


DM_2013_test <- DM_2013 %>%
  filter(Pollutant == "PM2.5")%>%
  filter(Site == "Deira")





# compare time series of data

DM_test <- DM %>%
  #  mutate(date = mdy_hms(DateTime, tz = "UTC")) %>%
  mutate(date = DateTime) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value) %>%
  filter(Site == "Deira")


DM_2013_test <- DM_2013 %>%
  #  mutate(date = mdy_hms(DateTime, tz = "UTC")) %>%
  mutate(date = DateTime) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value) %>%
  filter(Site == "Deira")


data_time_filtered <- DM_test %>%
  select(-Site)%>%
  spread(Pollutant, Value)


data_time_unfiltered <- DM_2013_test %>%
  select(-Site)%>%
  spread(Pollutant, Value)


# Build timeseries for plots
time_series_filtered <- data_frame_to_timeseries(data_time_filtered)
time_series_unfiltered <- data_frame_to_timeseries(data_time_unfiltered)

# Return
time_series_filtered
time_series_unfiltered




dawit<- ts.union( time_series_filtered, dframe = F)
All_data <- cbind(time_series_unfiltered,time_series_filtered)
All_data

dawit<- All_data["CO",]
  
  
# Get colour vector
colour_vector <- threadr::ggplot2_colours(45)

# bind two time series together
All_data <- cbind(time_series_unfiltered$NO2,time_series_filtered$NO2)

head(All_data)

plot <- dygraph(All_data, main = paste("Deira", " - ", "NO2")) %>%
  # dyOptions(colors = colour_vector[1]) %>%
  dySeries("..1",label = "with outliers") %>%
  dySeries("..2",label = "no outliers") %>%
  dyAxis("y", label = "Hourly PM<sub>2.5</sub> (&#956;g m<sup>-3</sup>)") %>%
  dyRangeSelector()

plot

# SO2

All_data_SO2 <- cbind(time_series_unfiltered$SO2,time_series_filtered$SO2)

head(All_data_SO2)

plot <- dygraph(All_data_SO2, main = paste("Deira", " - ", "SO2")) %>%
  # dyOptions(colors = colour_vector[1]) %>%
  dySeries("..1",label = "no outliers") %>%
  dySeries("..2",label = "with outliers") %>%
  dyAxis("y", label = "Hourly SO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>%
  dyRangeSelector()

plot


