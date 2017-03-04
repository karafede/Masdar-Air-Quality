library(dplyr)
library(readr)
library(threadr)
library(dygraphs)
library(tidyr)
library(ggplot2)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files")
source("new_boxplot_fun_daily.r")






# FEED ONLY ONE YEAR HOURLY DATA OF STATIONS FROM THE SAME AUTHORITY for example 2013 data from the Dubai Municipality (DM)
DM_2013 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_DM_2013_daily_mean.csv')
DM_2014 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_DM_2014_daily_mean.csv')
DM_2015 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_DM_2015_daily_mean.csv')
DM_2016 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_DM_2016_daily_mean.csv')
#DM_2014 <- read.csv('D:/database_DM_2014_hourly.csv')
#DM_2015 <- read.csv('D:/database_DM_2015_hourly.csv')
#DM_2016 <- read.csv('D:/database_DM_2016_hourly.csv')

# DM_month <-subset(DM_2013, DateTime > '2013-09-01 00:00:00 UTC' & DateTime < '2013-12-31 00:00:00 UTC')

# 3.5 range 
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016)
xx=0
for (aa in list_year){
  DM<- as.data.frame(aa)
  
  #DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
  #DM<-DM_2013
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
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
        output_OL <- walash_outlier(to_walash, 3.5) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        if (output_error==1 | output_error==2){      # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          # q=1 k<-ind_true[1] l=2
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
  file_name_out <- paste("daily_filtered_3_5_box/database_DM_", year_s, "_outliers.csv")
  file_name_data <- paste("daily_filtered_3_5_box/database_DM_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}

# with 4 range
remove(DM)
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016)
xx=0
for (aa in list_year){
  DM<- as.data.frame(aa)
  
  #DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
  #DM<-DM_2013
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
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
        output_OL <- walash_outlier(to_walash, 4) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        if (output_error==1 | output_error==2){      # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          # q=1 k<-ind_true[1] l=2
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
  file_name_out <- paste("daily_filtered_4_box/database_DM_", year_s, "_outliers.csv")
  file_name_data <- paste("daily_filtered_4_box/database_DM_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}



# end
#############################################################################
#############################################################################
remove(DM,DM_2013,DM_2014,DM_2015,DM_2016)

# FEED ONLY ONE YEAR HOURLY DATA OF STATIONS FROM THE SAME AUTHORITY for example 2013 data from the Dubai Municipality (DM)
DM_2013 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_EAD_2013_daily_mean.csv')
DM_2014 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_EAD_2014_daily_mean.csv')
DM_2015 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_EAD_2015_daily_mean.csv')
DM_2016 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_EAD_2016_daily_mean.csv')
#DM_2014 <- read.csv('D:/database_DM_2014_hourly.csv')
#DM_2015 <- read.csv('D:/database_DM_2015_hourly.csv')
#DM_2016 <- read.csv('D:/database_DM_2016_hourly.csv')

# DM_month <-subset(DM_2013, DateTime > '2013-09-01 00:00:00 UTC' & DateTime < '2013-12-31 00:00:00 UTC')

# 3.5 range 
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016)
xx=0
for (aa in list_year){
  DM<- as.data.frame(aa)
  
  #DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
  #DM<-DM_2013
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
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
        output_OL <- walash_outlier(to_walash, 3.5) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        if (output_error==1 | output_error==2){      # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          # q=1 k<-ind_true[1] l=2
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
  file_name_out <- paste("daily_filtered_3_5_box/database_EAD_", year_s, "_outliers.csv")
  file_name_data <- paste("daily_filtered_3_5_box/database_EAD_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}

# with 4 range
remove(DM)
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016)
xx=0
for (aa in list_year){
  DM<- as.data.frame(aa)
  
  #DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
  #DM<-DM_2013
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
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
        output_OL <- walash_outlier(to_walash, 4) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        if (output_error==1 | output_error==2){      # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          # q=1 k<-ind_true[1] l=2
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
  file_name_out <- paste("daily_filtered_4_box/database_EAD_", year_s, "_outliers.csv")
  file_name_data <- paste("daily_filtered_4_box/database_EAD_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}



# end
#############################################################################
#############################################################################
remove(DM,DM_2013,DM_2014,DM_2015,DM_2016)

# FEED ONLY ONE YEAR HOURLY DATA OF STATIONS FROM THE SAME AUTHORITY for example 2013 data from the Dubai Municipality (DM)
DM_2013 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_NCMS_2013_daily_mean.csv')
DM_2014 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_NCMS_2014_daily_mean.csv')
DM_2015 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_NCMS_2015_daily_mean.csv')
DM_2016 <- read_csv('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/Daily_mean/database_NCMS_2016_daily_mean.csv')
#DM_2014 <- read.csv('D:/database_DM_2014_hourly.csv')
#DM_2015 <- read.csv('D:/database_DM_2015_hourly.csv')
#DM_2016 <- read.csv('D:/database_DM_2016_hourly.csv')

# DM_month <-subset(DM_2013, DateTime > '2013-09-01 00:00:00 UTC' & DateTime < '2013-12-31 00:00:00 UTC')

# 3.5 range 
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016)
xx=0
for (aa in list_year){
  DM<- as.data.frame(aa)
  
  #DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
  #DM<-DM_2013
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
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
        output_OL <- walash_outlier(to_walash, 3.5) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        if (output_error==1 | output_error==2){      # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          # q=1 k<-ind_true[1] l=2
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
  file_name_out <- paste("daily_filtered_3_5_box/database_NCMS_", year_s, "_outliers.csv")
  file_name_data <- paste("daily_filtered_3_5_box/database_NCMS_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}

# with 4 range
remove(DM)
list_year<- list(DM_2013,DM_2014,DM_2015,DM_2016)
xx=0
for (aa in list_year){
  DM<- as.data.frame(aa)
  
  #DM<- rbind(DM_2013, DM_2014, DM_2015, DM_2016 )
  #DM<-DM_2013
  raw_sel <- subset(DM, select=c("Date", "Site","Pollutant","Daily_mean"))
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
        output_OL <- walash_outlier(to_walash, 4) # walash function which have two outcomes as a list
        output_error<- as.numeric(output_OL[2])
        if (output_error==1 | output_error==2){      # if output_error==1 there are outliers output_error==2 no outliers output_error==0 all of the values are NaN
          # REPLACING THE VALUES BY THE UPDATED DATA (filtered from outliers)
          output_OL_data<- as.data.frame(output_OL[1])
          dave <- (DM$Site==i & DM$Pollutant == j )
          ind_true <- which(dave)
          # q=1 k<-ind_true[1] l=2
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
  file_name_out <- paste("daily_filtered_4_box/database_NCMS_", year_s, "_outliers.csv")
  file_name_data <- paste("daily_filtered_4_box/database_NCMS_", year_s, "_daily_filtered.csv")
  write.csv(number_outliers, file = file_name_out)
  write.csv(DM, file = file_name_data)
  xx=xx+1
}



# end
#############################################################################
#############################################################################

