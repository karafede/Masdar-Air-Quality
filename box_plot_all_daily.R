library(dplyr)
library(readr)
library(lubridate)
library(dygraphs)
library(tidyr)
library(ggplot2)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/Scripts")
source("new_boxplot_fun_daily.r")
setwd('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean')


# FEED ONLY DATA FROM DM STATION
DM_2013 <- read_csv('database_DM_2013_daily_mean.csv')
DM_2014 <- read_csv('database_DM_2014_daily_mean.csv')
DM_2015 <- read_csv('database_DM_2015_daily_mean.csv')
DM_2016 <- read_csv('database_DM_2016_daily_mean.csv')
DM_2017 <- read_csv('database_DM_2017_daily_mean.csv')


# 3.5 range 
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016,DM_2017)
xx=0

for (aa in list_year){
  DM<- as.data.frame(aa)
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
  station <- unique(raw_sel$Site)
  david<- vector()
  number_outliers<-data.frame()
  
  for (i in station){
    stat<- filter(raw_sel, Site == i )
    pollu <- unique(stat$Pollutant)
    
    for(j in pollu){
      to_walash <- filter(stat, Pollutant == j )
      
      if (is.null(to_walash)){
      } else {
        output_OL <- walash_outlier(to_walash, 3.5) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
        if (output_error==1 | output_error==2){      
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          DM$Daily_mean[ind_true]<- output_OL_data$Daily_mean
          
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
  file_name_out <- paste("Daily_Filtered_3_5_Box/database_DM_", year_s, "_daily_outliers.csv")
  file_name_data <- paste("Daily_Filtered_3_5_Box/database_DM_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}

# with 4 range
remove(DM)
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016,DM_2017)
xx=0

for (aa in list_year){
  DM<- as.data.frame(aa)
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
  station <- unique(raw_sel$Site)
  david<- vector()
  number_outliers<-data.frame()
  
  for (i in station){
    stat<- filter(raw_sel, Site == i )
    pollu <- unique(stat$Pollutant)
    
    for(j in pollu){
      to_walash <- filter(stat, Pollutant == j )
      
      if (is.null(to_walash)){
      } else {
        output_OL <- walash_outlier(to_walash, 4) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
        if (output_error==1 | output_error==2){      
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          DM$Daily_mean[ind_true]<- output_OL_data$Daily_mean
          
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
  file_name_out <- paste("Daily_Filtered_4_Box/database_DM_", year_s, "_daily_outliers.csv")
  file_name_data <- paste("Daily_Filtered_4_Box/database_DM_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}


#############################################################################
#############################################################################

remove(DM,DM_2013,DM_2014,DM_2015,DM_2016,DM_2017)

# FEED ONLY DATA FROM EAD STATION 
DM_2013 <- read_csv('database_EAD_2013_daily_mean.csv')
DM_2014 <- read_csv('database_EAD_2014_daily_mean.csv')
DM_2015 <- read_csv('database_EAD_2015_daily_mean.csv')
DM_2016 <- read_csv('database_EAD_2016_daily_mean.csv')
DM_2017 <- read_csv('database_EAD_2017_daily_mean.csv')


# 3.5 range 
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016,DM_2017)
xx=0

for (aa in list_year){
  DM<- as.data.frame(aa)
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
  station <- unique(raw_sel$Site)
  david<- vector()
  number_outliers<-data.frame()
  
  for (i in station){
    stat<- filter(raw_sel, Site == i )
    pollu <- unique(stat$Pollutant)

    for(j in pollu){
      to_walash <- filter(stat, Pollutant == j )
      
      if (is.null(to_walash)){
      } else {
        output_OL <- walash_outlier(to_walash, 3.5) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
        if (output_error==1 | output_error==2){      
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          DM$Daily_mean[ind_true]<- output_OL_data$Daily_mean
          
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
  file_name_out <- paste("Daily_Filtered_3_5_Box/database_EAD_", year_s, "_daily_outliers.csv")
  file_name_data <- paste("Daily_Filtered_3_5_Box/database_EAD_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}

# with 4 range
remove(DM)
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016,DM_2017)
xx=0

for (aa in list_year){
  DM<- as.data.frame(aa)
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
  station <- unique(raw_sel$Site)
  david<- vector()
  number_outliers<-data.frame()
  
  for (i in station){
    stat<- filter(raw_sel, Site == i )
    pollu <- unique(stat$Pollutant)
    
    for(j in pollu){
      to_walash <- filter(stat, Pollutant == j )
      
      if (is.null(to_walash)){
      } else {
        output_OL <- walash_outlier(to_walash, 4) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
        if (output_error==1 | output_error==2){      
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          DM$Daily_mean[ind_true]<- output_OL_data$Daily_mean
          
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
  file_name_out <- paste("Daily_Filtered_4_Box/database_EAD_", year_s, "_daily_outliers.csv")
  file_name_data <- paste("Daily_Filtered_4_Box/database_EAD_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}


#############################################################################
#############################################################################

remove(DM,DM_2013,DM_2014,DM_2015,DM_2016,DM_2017)

# FEED ONLY DATA FROM NCMS STATION
DM_2013 <- read_csv('database_NCMS_2013_daily_mean.csv')
DM_2014 <- read_csv('database_NCMS_2014_daily_mean.csv')
DM_2015 <- read_csv('database_NCMS_2015_daily_mean.csv')
DM_2016 <- read_csv('database_NCMS_2016_daily_mean.csv')
DM_2017 <- read_csv('database_NCMS_2017_daily_mean.csv')


# 3.5 range 
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016,DM_2017)
xx=0

for (aa in list_year){
  DM<- as.data.frame(aa)
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
  station <- unique(raw_sel$Site)
  david<- vector()
  number_outliers<-data.frame()
  
  for (i in station){
    stat<- filter(raw_sel, Site == i )
    pollu <- unique(stat$Pollutant)

    for(j in pollu){
      to_walash <- filter(stat, Pollutant == j )
      
      if (is.null(to_walash)){
      } else {
        output_OL <- walash_outlier(to_walash, 3.5) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
        if (output_error==1 | output_error==2){      
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          DM$Daily_mean[ind_true]<- output_OL_data$Daily_mean
          
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
  file_name_out <- paste("Daily_Filtered_3_5_Box/database_NCMS_", year_s, "_daily_outliers.csv")
  file_name_data <- paste("Daily_Filtered_3_5_Box/database_NCMS_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}

# with 4 range
remove(DM)
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016,DM_2017)
xx=0

for (aa in list_year){
  DM<- as.data.frame(aa)
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
  station <- unique(raw_sel$Site)
  david<- vector()
  number_outliers<-data.frame()
  
  for (i in station){
    stat<- filter(raw_sel, Site == i )
    pollu <- unique(stat$Pollutant)

    for(j in pollu){
      to_walash <- filter(stat, Pollutant == j )
      
      if (is.null(to_walash)){
      } else {
        output_OL <- walash_outlier(to_walash, 4) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
        if (output_error==1 | output_error==2){      
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          DM$Daily_mean[ind_true]<- output_OL_data$Daily_mean
          
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
  file_name_out <- paste("Daily_Filtered_4_Box/database_NCMS_", year_s, "_daily_outliers.csv")
  file_name_data <- paste("Daily_Filtered_4_Box/database_NCMS_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}