library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(compare)
library(tidyr)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data")
source("mean_na.r")

EAD_data <- read_csv("database_EAD_2016_daily.csv")

# replace NaN (not a Number with NA that is a missing value)
EAD_data[sapply(EAD_data,is.na)] = NA 
# EAD_data <- na.omit(EAD_data)

percentile_EAD_data <- EAD_data %>%
  dplyr::group_by(Pollutant,
                  Site) %>%
  summarise("98 percentile" = quantile(Value, c(0.98),  na.rm = TRUE),
            "97 percentile" = quantile(Value, c(0.97),  na.rm = TRUE),
            "96 percentile" = quantile(Value, c(0.96),  na.rm = TRUE),
            "95 percentile" = quantile(Value, c(0.95),  na.rm = TRUE))



# function to make quartelry averages---------------------------------------
quarterly_averages <- function (data, site) {
  
  data <- data %>%
    mutate(date = mdy(date, tz = "UTC"),
           year = year(date),
           month = month(date)) %>%
    dplyr:: select(date,
                   year,
                   month,
                   Pollutant,
                   Value,
                   Cap) %>%
    filter(Pollutant == "PM2.5" & site == site)
  
  
  ## get the months of observations
  data$month <- factor(format(data$date, format = "%b"), levels = month.abb)
  
  ## Format the quarters
  data$quarter <- character(length = nrow(data))
  data$quarter[data$month %in% month.abb[c(1:3)]] <- "Q1"
  data$quarter[data$month %in% month.abb[c(4:6)]] <- "Q2"
  data$quarter[data$month %in% month.abb[c(7:9)]] <- "Q3"
  data$quarter[data$month %in% month.abb[c(10:12)]] <- "Q4"
  data$quarter <- factor(data$quarter, levels = c("Q1","Q2","Q3","Q4"))
  
  
  ## year variable
  data$year <- factor(format(data$date, format = "%Y"))
  
  ## and aggregate
  
  AVG_QUARTERS <- with(data[,], aggregate(Value, list(quarter = quarter,
                                                      year = year), FUN = mean, na.rm = TRUE))
  
  
  names(AVG_QUARTERS)[names(AVG_QUARTERS) == 'x'] <- 'Quarters'
  
  
  # write.csv(AVG_QUARTERS, paste(site,"_quarterly_AVG.csv", sep = ""), row.names=FALSE)
  Annual_mean <- mean(AVG_QUARTERS$Quarters)
  AAA <- data.frame(c("site", "year", "Pollutant", "Annual Mean"),
                    c(site, 2015, "PM2.5",Annual_mean))
  
  AAA <- as.data.frame (t(AAA))
  colnames(AAA) <- as.character(unlist(AAA[1,]))
  AAA = AAA[-1, ]
  row.names(AAA) <- NULL
  
  # write.csv(Annual_mean, paste(site,"_annual_mean.csv", sep = ""), row.names=FALSE)
}


AVG_QUARTERS <- quarterly_averages(EAD_data, "Hamdan Street")

# Annual_mean <- mean(AVG_QUARTERS$Quarters)








###############################################################################
###############################################################################
# data capture


EAD_data <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/database_EAD_2015_hourly.csv")

# replace NaN (not a Number with NA that is a missing value)
EAD_data[sapply(EAD_data,is.na)] = NA 
# EAD_data <- na.omit(EAD_data)

# function to make quartelry averages---------------------------------------
# quarters <- function (data) {
# remove(data)

data <- EAD_data %>%
  mutate(date = DateTime,
         year = year(DateTime),
         day= day(DateTime),
      hour =  hour(DateTime + 300),
        # hour=round(hour(DateTime), units="hours")(hour(DateTime)),
         month = month(DateTime)) %>%
  dplyr:: select(date,
                 Site,
                 year,
                 month,
                 day,
                 hour,
                 Pollutant,
                 Value) %>%
  filter(Pollutant == "O3") # select the pollutant 


# the 8 hour moving average 
 Cap_h<-array()
# NAN_values = which(is.na(data$Value), arr.ind = FALSE, useNames = TRUE)
# not_NAN=which(!(is.na(data$Value)), arr.ind = FALSE, useNames = TRUE)
# Cap_h[NAN_values]=0
# Cap_h[not_NAN]=1
check_list<- seq(
  from=as.POSIXct("2015-1-1 0:00", tz="UTC"),
  to=as.POSIXct("2015-12-31 23:00", tz="UTC"),
  by="hour") 
check_list<-as.data.frame(check_list)
check_list <- check_list %>%
  mutate(year = year(check_list),
         month = month(check_list),
         day= day(check_list),
         hour = hour(check_list ))
         # hour=round(hour(DateTime), units="hours")(hour(DateTime)),
         
# select by stations to loop 
average_O3_all<- data.frame()
for (j in da<-unique(data$Site)){
  station_o3= filter( data, Site == j )
  NAN_values = which(is.na(station_o3$Value), arr.ind = FALSE, useNames = TRUE)
  not_NAN=which(!(is.na(station_o3$Value)), arr.ind = FALSE, useNames = TRUE)
  Cap_h[NAN_values]=0
  Cap_h[not_NAN]=1
  for (i in 1:nrow(station_o3)){
    #if (data$hour[1:i]== check_list$hour[1:i])
    

    # a1 <- data.frame(a = 1:5, b = letters[1:5])
    # a2 <- data.frame(a = 1:3, b = letters[1:3])
    # comparison <- compare(a1,a2,allowAll=TRUE)
    if (i < 8){
      xx= station_o3$Value[1:i]*Cap_h[1:i]
      if (sum(Cap_h[1:i])/8 >= 0.75){
        yy=sum(xx)/sum(Cap_h[1:i])
        yy_cap=sum(Cap_h[1:i])/8*100
      }else{
        yy=NA
        yy_cap=sum(Cap_h[1:i])/8*100
      }
    }
    if (i >= 8){
      xx= station_o3$Value[(i-7):i]*Cap_h[(i-7):i]
      if (sum(Cap_h[(i-7):i])/8 >= 0.75){
        yy=sum(xx,na.rm = TRUE)/sum(Cap_h[(i-7):i])
        yy_cap=sum(Cap_h[(i-7):i])/8*100
      }else{
        yy=NA
        yy_cap=sum(Cap_h[(i-7):i])/8*100
      }
    }

  average_O3<- data.frame (Date=check_list[i, 1] ,Site=j, Pollutant="O3", Value=yy, Capture=yy_cap) 
  average_O3_all <- rbind(average_O3_all,average_O3)
  remove(average_O3)
  }
    
}


# the daily average of ozone in all the stations
data_time <- average_O3_all %>%
  select(Date , Site,Pollutant, Value )%>%
  spread(Site, Value)

dawit_da<-data_time %>%
  mutate(date_gr= format(Date, format="%Y-%m-%d")) %>%
  select(-Date, -Pollutant)%>%
  group_by(date_gr) %>%
  summarise_all(funs(mean_na))


# the daily capture of the stations
dawit_da_cap<-data_time %>%
  mutate(date_gr= format(Date, format="%Y-%m-%d")) %>%
  select(-Date, -Pollutant)%>%
  group_by(date_gr) %>%
  summarise_all(funs(count_na))


# getting the mean daily average according to the EPA statndard i.e. stations must have 75% or more data on daily baisis to estimate the mean 

daily_data<- select(dawit_da,-date_gr)
daily_capture<-select(dawit_da_cap,-date_gr)


for (i in 1:ncol(daily_data)){
  check_T_F<-daily_capture[,i] >= 75
  ind_false <- which(!(check_T_F))
  daily_data[ind_false,i]<- NA
}
daily_data$Date <-dawit_da$date_gr

daily_O3<-gather(daily_data, "Site" , "Value" , -Date)
daily_O3_cap<-gather(daily_capture, "Site" , "Capture" )

daily_O3$Capture<-daily_O3_cap$Capture

# exporting the daily O3 data 
write()

write_csv(daily_O3, "database_EAD_2015_Daily_O3.csv", na = "NA", delim = ",")






check_T_F<-daily_capture >= 75
ind_false <- which(!(check_T_F))
daily_data<- NA



 remove(NAN_values, not_NAN, i, j, check_list, Cap_h)

length(check_list)

start_a=as.Date("2015/01/01 00:00:00", format='%Y/%m/%d hh:mm:ss')
end_a=as.Date("2015/12/31 23:00:00", format='%Y/%m/%d hh:mm:ss')







Cap_h[]
data <- data %>%
  mutate(Cap_h[NAN_values] = 0)


for (i in 1:nrow(data)){
  if (is.na(data$Value[i])){
    data$NAN_value[i] <- 0
  } else{
    data$NAN_value[i] <-  1
  }
}

ave_8_hr<- data %>%
  group_by(Site,
           year,
           month,
           day) %>%
  summarise(SUM = 100*sum(Cap_bin)/sum(one))




## get the months of observations
data$month <- factor(format(data$date, format = "%b"), levels = month.abb)

## Define the quarters the quarters
data$quarter <- character(length = nrow(data))
data$quarter[data$month %in% month.abb[c(1:3)]] <- "Q1"
data$quarter[data$month %in% month.abb[c(4:6)]] <- "Q2"
data$quarter[data$month %in% month.abb[c(7:9)]] <- "Q3"
data$quarter[data$month %in% month.abb[c(10:12)]] <- "Q4"
data$quarter <- factor(data$quarter, levels = c("Q1","Q2","Q3","Q4"))


aaa <- data$Cap < 75
dawit <- which(aaa, arr.ind = TRUE, useNames = TRUE)
data$Value[dawit]<- NA

data$Cap_bin <- as.data.frame()
for (i in 1:nrow(data)){
  if (data$Cap[i] >= 75){
    data$Cap_bin[i] <- 1
  } 
  if (data$Cap[i] < 75 ){
    data$Cap_bin[i]<-0
  }
}
data$one <-1



capture_quarter <- data %>%
  group_by(Site,
           quarter) %>%
  summarise(SUM = 100*sum(Cap_bin)/sum(one))






