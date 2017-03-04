
library(dplyr)
library(readr)
library(threadr)
library(dygraphs)
library(tidyr)
library(ggplot2)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files")
source("walash_fuunction_copy.r")

# FEED ONLY ONE YEAR HOURLY DATA OF STATIONS FROM THE SAME AUTHORITY for example 2013 data from the Dubai Municipality (DM)
DM_2013 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_DM_2013_hourly.csv')
DM_2014 <- read.csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_DM_2014_hourly.csv')
DM_2015 <- read.csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_DM_2015_hourly.csv')
DM_2016 <- read.csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_DM_2016_hourly.csv')

# check data distribution
list_year<- list(DM_2013,DM_2014,DM_2015 ,DM_2016)
xx=0
for (aa in list_year){
  # DM_2013_PM25 <- DM_2013 %>%
  #   filter(Pollutant == "PM2.5")
  # 
  # DM_2013_PM25 %>%  
  #   ggplot(aes(Value, ..count..)) + 
  #   geom_density(fill = "dodgerblue", alpha = .5) +
  #   theme_bw()
  # 
  # DM_month <-subset(DM_2013, DateTime > '2013-09-01 00:00:00 UTC' & DateTime < '2013-12-31 00:00:00 UTC')
  
  #DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
  DM<- as.data.frame(aa)
  raw_sel <- subset(DM, select=c("DateTime", "Site","Pollutant","Value"))
  station <- unique(raw_sel$Site)
  david<- vector()
  number_outliers<-data.frame()
  number_Sus<-data.frame()
  number_Large<-data.frame()
  number_Small<-data.frame()
  number_Lar_Pl<-data.frame()
  number_Sma_Pl<- data.frame()
  for (i in station){
    # i<-"Baniyas School" # to be deleted
    stat<- filter(raw_sel, Site == i )
    pollu <- unique(stat$Pollutant)
    #number_out<-data.frame()
    
    for(j in pollu){
      #  j<-"NO2"# to be deleted
      to_walash <- filter(stat, Pollutant == j )
      
      if (is.null(to_walash)){
      } else {
        output_OL <- walash_outlier(to_walash) # walash function which have two outcomes as a list
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
          # the number of X small
          outlier_num_ss<-as.numeric(output_OL[5])
          number_out_small<- data.frame(Site=i, Pollutant=j, Number_Outliers=outlier_num_ss) 
          number_Small<-rbind(number_Small,number_out_small)
          remove(number_out_small)
          # the number of X large
          outlier_num_ll<-as.numeric(output_OL[6])
          number_out_large<- data.frame(Site=i, Pollutant=j, Number_Outliers=outlier_num_ll) 
          number_Large<-rbind(number_Large,number_out_large)
          remove(number_out_large)
          # the number of sus
          outlier_sus<-as.numeric(output_OL[4])
          number_out_sus<- data.frame(Site=i, Pollutant=j, Number_Outliers=outlier_sus) 
          number_Sus<-rbind(number_Sus,number_out_sus)
          remove(number_out_sus)
          
          # the number of Large Plateau
          outlier_ll_pl<-as.numeric(output_OL[7])
          number_ll_pl<- data.frame(Site=i, Pollutant=j, Number_L_plateau=outlier_ll_pl) 
          number_Lar_Pl<-rbind(number_Lar_Pl,number_ll_pl)
          remove(number_ll_pl)
          
          # the number of Small Plateau
          outlier_ss_pl<-as.numeric(output_OL[8])
          number_ss_pl<- data.frame(Site=i, Pollutant=j, Number_S_plateau=outlier_ss_pl) 
          number_Sma_Pl<-rbind(number_Sma_Pl,number_ss_pl)
          remove(number_ss_pl)
          
          
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
  file_name_out <- paste("Walsh_test/database_DM_", year_s, "_outliers.csv", sep="")
  file_name_data <- paste("Walsh_test/database_DM_", year_s, "_hourly_filtered.csv", sep="")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}

###### delete everything

rm(list = ls())
#########

library(dplyr)
library(readr)
library(threadr)
library(dygraphs)
library(tidyr)
library(ggplot2)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files")
source("walash_fuunction_copy.r")


##### NCMS

# FEED ONLY ONE YEAR HOURLY DATA OF STATIONS FROM THE SAME AUTHORITY for example 2013 data from the Dubai Municipality (DM)
DM_2013 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_NCMS_2013_hourly.csv')
DM_2014 <- read.csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_NCMS_2014_hourly.csv')
DM_2015 <- read.csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_NCMS_2015_hourly.csv')
DM_2016 <- read.csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_NCMS_2016_hourly.csv')

# check data distribution
list_year<- list(DM_2013,DM_2014,DM_2015 ,DM_2016)
xx=0
for (aa in list_year){
  
  #DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
  DM<- as.data.frame(aa)
  raw_sel <- subset(DM, select=c("DateTime", "Site","Pollutant","Value"))
  station <- unique(raw_sel$Site)
  david<- vector()
  number_outliers<-data.frame()
  number_Sus<-data.frame()
  number_Large<-data.frame()
  number_Small<-data.frame()
  number_Lar_Pl<-data.frame()
  number_Sma_Pl<- data.frame()
  for (i in station){
    # i<-"Baniyas School" # to be deleted
    stat<- filter(raw_sel, Site == i )
    pollu <- unique(stat$Pollutant)
    #number_out<-data.frame()
    
    for(j in pollu){
      #  j<-"NO2"# to be deleted
      to_walash <- filter(stat, Pollutant == j )
      
      if (is.null(to_walash)){
      } else {
        output_OL <- walash_outlier(to_walash) # walash function which have two outcomes as a list
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
          # the number of X small
          outlier_num_ss<-as.numeric(output_OL[5])
          number_out_small<- data.frame(Site=i, Pollutant=j, Number_Outliers=outlier_num_ss) 
          number_Small<-rbind(number_Small,number_out_small)
          remove(number_out_small)
          # the number of X large
          outlier_num_ll<-as.numeric(output_OL[6])
          number_out_large<- data.frame(Site=i, Pollutant=j, Number_Outliers=outlier_num_ll) 
          number_Large<-rbind(number_Large,number_out_large)
          remove(number_out_large)
          # the number of sus
          outlier_sus<-as.numeric(output_OL[4])
          number_out_sus<- data.frame(Site=i, Pollutant=j, Number_Outliers=outlier_sus) 
          number_Sus<-rbind(number_Sus,number_out_sus)
          remove(number_out_sus)
          
          # the number of Large Plateau
          outlier_ll_pl<-as.numeric(output_OL[7])
          number_ll_pl<- data.frame(Site=i, Pollutant=j, Number_L_plateau=outlier_ll_pl) 
          number_Lar_Pl<-rbind(number_Lar_Pl,number_ll_pl)
          remove(number_ll_pl)
          
          # the number of Small Plateau
          outlier_ss_pl<-as.numeric(output_OL[8])
          number_ss_pl<- data.frame(Site=i, Pollutant=j, Number_S_plateau=outlier_ss_pl) 
          number_Sma_Pl<-rbind(number_Sma_Pl,number_ss_pl)
          remove(number_ss_pl)
          
          
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
  file_name_out <- paste("Walsh_test/database_NCMS_", year_s, "_outliers.csv", sep="")
  file_name_data <- paste("Walsh_test/database_NCMS_", year_s, "_hourly_filtered.csv", sep="")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}




# station

# dawit_DG<-output_OL[7]
# 
# 
# DM_test <- DM %>%
#   filter(Site == "Deira") %>%
#   filter(Pollutant == "PM2.5")
# 
# 
# DM_2013_test <- DM_2013 %>%
#   filter(Pollutant == "PM2.5")%>%
#   filter(Site == "Deira")
# 
# 
# 
# 
#  
# # compare time series of data
# 
# DM_test <- DM %>%
#   #  mutate(date = mdy_hms(DateTime, tz = "UTC")) %>%
#   mutate(date = DateTime) %>%
#   dplyr:: select(date,
#                  Site,
#                  Pollutant,
#                  Value) %>%
#   filter(Site == "Deira")
# 
# 
# DM_2013_test <- DM_2013 %>%
#   #  mutate(date = mdy_hms(DateTime, tz = "UTC")) %>%
#   mutate(date = DateTime) %>%
#   dplyr:: select(date,
#                  Site,
#                  Pollutant,
#                  Value) %>%
#   filter(Site == "Deira")
# 
# 
# data_time_filtered <- DM_test %>%
#   spread(Pollutant, Value)
# 
# 
# data_time_unfiltered <- DM_2013_test %>%
#   spread(Pollutant, Value)
# 
# 
# # Build timeseries for plots
# time_series_filtered <- data_frame_to_timeseries(data_time_filtered)
# time_series_unfiltered <- data_frame_to_timeseries(data_time_unfiltered)
# 
# # Return
# time_series_filtered
# time_series_unfiltered
# 
# 
#   
#   # Get colour vector
#   colour_vector <- threadr::ggplot2_colours(45)
#   
#   # bind two time series together
#   All_data <- cbind(time_series_unfiltered$PM2.5,time_series_filtered$PM2.5)
#   
#   head(All_data)
#   
#   plot <- dygraph(All_data, main = paste("Deira", " - ", "PM2.5")) %>% 
#    # dyOptions(colors = colour_vector[1]) %>% 
#     dySeries("..1",label = "no outliers") %>% 
#     dySeries("..2",label = "with outliers") %>%  
#     dyAxis("y", label = "Hourly PM<sub>2.5</sub> (&#956;g m<sup>-3</sup>)") %>% 
#     dyRangeSelector()
#   
#   plot
#   
# # SO2
#   
#   All_data_SO2 <- cbind(time_series_unfiltered$SO2,time_series_filtered$SO2)
#   
#   head(All_data_SO2)
#   
#   plot <- dygraph(All_data_SO2, main = paste("Deira", " - ", "SO2")) %>% 
#     # dyOptions(colors = colour_vector[1]) %>% 
#     dySeries("..1",label = "no outliers") %>% 
#     dySeries("..2",label = "with outliers") %>%  
#     dyAxis("y", label = "Hourly SO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>% 
#     dyRangeSelector()
#   
#   plot
# 
# write.csv(number_outliers, file = "database_DM_2013_outliers.csv")
# write.csv(DM, file = "database_DM_2013_hourly_filtered.csv")
