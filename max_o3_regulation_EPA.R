

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)

# importing the data
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3")

data_NCMS_o3_2013<- read_csv("database_NCMS_2013_O3_daily.csv")
data_NCMS_o3_2014<- read_csv("database_NCMS_2014_O3_daily.csv")
data_NCMS_o3_2015<- read_csv("database_NCMS_2015_O3_daily.csv")
data_NCMS_o3_2016<- read_csv("database_NCMS_2016_O3_daily.csv")
data_NCMS_o3_2017<- read_csv("database_NCMS_2017_O3_daily.csv")

data_DM_o3_2013<- read_csv("database_DM_2013_O3_daily.csv")
data_DM_o3_2014<- read_csv("database_DM_2014_O3_daily.csv")
data_DM_o3_2015<- read_csv("database_DM_2015_O3_daily.csv")
data_DM_o3_2016<- read_csv("database_DM_2016_O3_daily.csv")
data_DM_o3_2017<- read_csv("database_DM_2017_O3_daily.csv")

data_EAD_o3_2013<- read_csv("database_EAD_2013_O3_daily.csv")
data_EAD_o3_2014<- read_csv("database_EAD_2014_O3_daily.csv")
data_EAD_o3_2015<- read_csv("database_EAD_2015_O3_daily.csv")
data_EAD_o3_2016<- read_csv("database_EAD_2016_O3_daily.csv")
data_EAD_o3_2017<- read_csv("database_EAD_2017_O3_daily.csv")

data_o3<-rbind(data_NCMS_o3_2013,data_NCMS_o3_2014,data_NCMS_o3_2015,data_NCMS_o3_2016,data_NCMS_o3_2017,
               data_DM_o3_2013,data_DM_o3_2014,data_DM_o3_2015,data_DM_o3_2016,data_DM_o3_2017,
               data_EAD_o3_2013,data_EAD_o3_2014,data_EAD_o3_2015,data_EAD_o3_2016,data_EAD_o3_2017)
# removing the values with less than 75% capture

data_o3$MAX_8hour[data_o3$Capture < 75] <- NA

daily_list_station<- data_o3 %>%
  mutate(date_year= year(Date)) %>%
  #group_by(Site) %>%
  split(data_o3$Site)

# spreading
field_name<- names(daily_list_station)
for (i in (1:length(field_name) )){
  dawit <- as.data.frame(daily_list_station[i])
  colnames(dawit) <- c("Date","Site","Mean_8hour","MAX_8hour","Capture", "date_year")
  
  dawit$MAX_8hour <- as.numeric(dawit$MAX_8hour)
  dawit$Mean_8hour <- as.numeric(dawit$Mean_8hour)
  #  
   sorted_one<- dawit %>%
    select(Date,Site,MAX_8hour, date_year)%>%
    spread(date_year, MAX_8hour) 
   
   
   sorted_max_2013 <- sort(sorted_one$`2013`, decreasing=T)[1:5] 
  sorted_max_2014 <- sort(sorted_one$`2014`, decreasing=T)[1:5]
   sorted_max_2015 <- sort(sorted_one$`2015`, decreasing=T)[1:5]
   sorted_max_2016 <- sort(sorted_one$`2016`, decreasing=T)[1:5]
   sorted_max_2017 <- sort(sorted_one$`2017`, decreasing=T)[1:5]
  max_table<-cbind(sorted_max_2013,sorted_max_2014,sorted_max_2015,sorted_max_2016,sorted_max_2017)/1960
   
  max_table<-as.data.frame(cbind(rbind(field_name[i],field_name[i],field_name[i],field_name[i],field_name[i]),max_table))
  max_4_table<-as.data.frame(max_table[4,])
  
  
  # similarly for the capture
 
 Cap_max_2013 <- !is.na(sorted_one$`2013`[1:365])
Cap_max_2013[Cap_max_2013 == T] <- 1
Cap_2013ann <- sum(Cap_max_2013) *100/365

Cap_max_2014 <- !is.na(sorted_one$`2014`[366:730])
Cap_max_2014[Cap_max_2014 == T] <- 1
Cap_2014ann <- sum(Cap_max_2014) *100/365

Cap_max_2015 <- !is.na(sorted_one$`2015`[731:1095])
Cap_max_2015[Cap_max_2015 == T] <- 1
Cap_2015ann <- sum(Cap_max_2015) *100/365

Cap_max_2016 <- !is.na(sorted_one$`2016`[1096:1461])
Cap_max_2016[Cap_max_2016 == T] <- 1
Cap_2016ann <- sum(Cap_max_2016) *100/366

Cap_max_2017 <- !is.na(sorted_one$`2017`[1462:1826])
Cap_max_2017[Cap_max_2017 == T] <- 1
Cap_2017ann <- sum(Cap_max_2017) *100/365

cap_table <- cbind(Cap_2013ann,Cap_2014ann,Cap_2015ann,Cap_2016ann,Cap_2017ann)
cap_table<-cbind(field_name[i],cap_table)


if (i!=1 ){
  write.table(max_table, file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_5_values_o3.csv",sep = ",",col.names = F,row.names = F, append = TRUE)
  write.table(max_4_table, file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_4_values_o3.csv",sep = ",",col.names = F,row.names = F, append = TRUE)
  write.table(cap_table, file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Cap_4_values_o3.csv",sep = ",",col.names = F,row.names = F, append = TRUE)
} else if ( file.exists("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_5_values_o3.csv") & file.exists("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_4_values_o3.csv") & file.exists("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Cap_4_values_o3.csv")){
  write.table(max_table, file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_5_values_o3.csv",sep = ",",col.names = F,row.names = F, append = TRUE)
  write.table(max_4_table, file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_4_values_o3.csv",sep = ",",col.names = F,row.names = F, append = TRUE)
  write.table(cap_table, file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Cap_4_values_o3.csv",sep = ",",col.names = F,row.names = F, append = TRUE)
}else{
  write.table(max_table, file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_5_values_o3.csv",sep = ",",col.names = T,row.names = F, append = F)
  write.table(max_4_table, file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_4_values_o3.csv",sep = ",",col.names = T,row.names = F, append = F)
  write.table(cap_table, file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Cap_4_values_o3.csv", sep = ",",col.names = T,row.names = F, append = F)
}

rm(list= setdiff(ls(),c( "daily_list_station","field_name", "i", "data_o3")))

}


