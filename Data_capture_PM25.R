
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/")
# setwd("C:/Users/fkaragulian/Dropbox/daily data/")

# data capture --------------------------------------------------

 EAD_data_2013 <- read_csv("Daily filtered with 4 boxplot/database_EAD_ 2013 _daily_filtered.csv")
 EAD_data_2014 <- read_csv("Daily filtered with 4 boxplot/database_EAD_ 2014 _daily_filtered.csv")
 EAD_data_2015 <- read_csv("Daily filtered with 4 boxplot/database_EAD_ 2015 _daily_filtered.csv")
 EAD_data_2016 <- read_csv("Daily filtered with 4 boxplot/database_EAD_ 2016 _daily_filtered.csv")
 
 DM_data_2013 <- read_csv("Daily filtered with 4 boxplot/database_DM_ 2013 _daily_filtered.csv")
 DM_data_2014 <- read_csv("Daily filtered with 4 boxplot/database_DM_ 2014 _daily_filtered.csv")
 DM_data_2015 <- read_csv("Daily filtered with 4 boxplot/database_DM_ 2015 _daily_filtered.csv")
 DM_data_2016 <- read_csv("Daily filtered with 4 boxplot/database_DM_ 2016 _daily_filtered.csv")
 
 NCMS_data_2013 <- read_csv("Daily filtered with 4 boxplot/database_NCMS_ 2013 _daily_filtered.csv")
 NCMS_data_2014 <- read_csv("Daily filtered with 4 boxplot/database_NCMS_ 2014 _daily_filtered.csv")
 NCMS_data_2015 <- read_csv("Daily filtered with 4 boxplot/database_NCMS_ 2015 _daily_filtered.csv")
 NCMS_data_2016 <- read_csv("Daily filtered with 4 boxplot/database_NCMS_ 2016 _daily_filtered.csv")

AQ_data <- rbind(EAD_data_2013, EAD_data_2014, EAD_data_2015, EAD_data_2016, 
                 DM_data_2013, DM_data_2014, DM_data_2015, DM_data_2016,
                 NCMS_data_2013, NCMS_data_2014, NCMS_data_2015, NCMS_data_2016)


# replace NaN (not a Number with NA that is a missing value)
#### changing the names of the colomns and deleting unwanted fields

AQ_data[sapply(AQ_data,is.na)] = NA
colnames(AQ_data)[12] <- "Cap"
AQ_data<- AQ_data %>%
  select(-X1, -X1_1)

# get the name of the sites
# names_2 <- AQ_data %>% 
#   select(Site,
#          Daily_mean) %>%
#   group_by(Site) %>%
#   summarise(mean = mean(Daily_mean, na.rm = TRUE))
# 
# list_names <- as.list(names_2$Site)

#### dawit added
names<-as.list(unique(AQ_data$Site))

####  
  
# filter only PM2.5 by station and by year and calculate data capture by quarter


data <- AQ_data %>%
  mutate(date = ymd(Date, tz = "UTC"),
         year = year(date),
         month = month(date)) %>%
  dplyr:: select(date,
                 Site,
                 year,
                 month,
                 Pollutant,
                 Daily_mean,
                 Cap) %>%
  filter(Pollutant == "PM2.5")


## get the months of observations
data$month <- factor(format(data$date, format = "%b"), levels = month.abb)

## Define the quarters the quarters
data$quarter <- character(length = nrow(data))
data$quarter[data$month %in% month.abb[c(1:3)]] <- "Q1"
data$quarter[data$month %in% month.abb[c(4:6)]] <- "Q2"
data$quarter[data$month %in% month.abb[c(7:9)]] <- "Q3"
data$quarter[data$month %in% month.abb[c(10:12)]] <- "Q4"
data$quarter <- factor(data$quarter, levels = c("Q1","Q2","Q3","Q4"))

# all data with data capture < 75% are set to NA
aaa <- data$Cap < 75
dawit <- which(aaa, arr.ind = TRUE, useNames = TRUE)
data$Daily_mean[dawit]<- NA

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

# look at data capture in each quarter
capture_quarter <- data %>%
  group_by(Site,
           quarter,
           year) %>%
  summarise(SUM = round(100*sum(Cap_bin)/sum(one), digits = 0))
capture_quarter_row <- capture_quarter

# spread data
capture_quarter <- capture_quarter %>%
spread(quarter, SUM)

#### write_csv(capture_quarter, "Capture_PM25.csv")

###############################################################################
# function to all quarterly averages------------------------------------------
# data<-AQ_data
# site<- unlist(names[20])
quarterly_averages <- function (data, site) {
  
  data <- data %>%
    mutate(date = ymd(Date, tz = "UTC"),
           year = year(date),
           month = month(date)) %>%
    dplyr:: select(date,
                   year,
                   month,
                   Pollutant,
                   Daily_mean,
                   Cap,
                   Site) %>%
    filter(Pollutant == "PM2.5"  & Site == site)
  
  ## get the months of observations
  
  data$month <- factor(format(data$date, format = "%b"), levels = month.abb)
  
  
  ## Format the quarters
  data$quarter <- character(length = nrow(data))
  data$quarter[data$month %in% month.abb[c(1:3)]] <- "Q1"
  data$quarter[data$month %in% month.abb[c(4:6)]] <- "Q2"
  data$quarter[data$month %in% month.abb[c(7:9)]] <- "Q3"
  data$quarter[data$month %in% month.abb[c(10:12)]] <- "Q4"
  data$quarter <- factor(data$quarter, levels = c("Q1","Q2","Q3","Q4"))
  
  # all data with data capture < 75% are set to NA
  aaa <- data$Cap < 75
  dawit <- which(aaa, arr.ind = TRUE, useNames = TRUE)
  data$Daily_mean[dawit]<- NA
  
  ## year variable
  data$year <- factor(format(data$date, format = "%Y"))
  
  # make averages by quarters------------------------------------------
  AVG_quarter <- data %>%
    group_by(Site,
             quarter,
             year) %>%
    summarise(mean = round(mean(Daily_mean, na.rm = TRUE), digits = 2))
  
  # return
  AVG_quarter <- as.data.frame(AVG_quarter)
  AVG_quarter
  
}


# loop all the stations and concatenate all the data to calcualte quarterly means
All_quarters <- data.frame()
for (i in 1:length(names)) {
  # All_AVG <- quarterly_averages(AQ_data, unlist(list_names[6]))
  All_AVG <- quarterly_averages(AQ_data, unlist(names[i]))
  All_quarters <- rbind(All_quarters, All_AVG)
}

# bind quartely data with the data capture by quarters
All_quarters$cap <- capture_quarter_row$SUM

# filter only data with cap >= 75
All_quarters <- All_quarters %>%
  filter(cap == 75 | cap > 75 )

All_quarters <- All_quarters %>%
  select(quarter,
         Site,
         year,
         mean)


# spread data
All_quarters <- All_quarters %>%
  spread(quarter, mean)


All_quarters$annual_AVG <- round(rowMeans(All_quarters[ ,3:6], na.rm = TRUE), digit = 2)

# replace NaN (not a Number with NA that is a missing value)
All_quarters[sapply(All_quarters,is.na)] = NA

write.csv(All_quarters, "Annual_means_Quarters_PM2_5_new.csv", row.names=FALSE)


#### three year average 

# stations that have three year of data

station_3y <- All_quarters %>%
  select(Site, year, annual_AVG)
dawit<- station_3y %>%
  spread(Site, annual_AVG)
# removing 2013 because of all values are NA

dawit<- dawit[-1,]

# 
mean_3y<-lapply(dawit, mean, na.rm = T)

mean_3y_df<- as.data.frame(mean_3y)

# removing year
mean_3y_df<- mean_3y_df[,-1]

mean_3year<-gather(mean_3y_df, key= "Site" , value="mean")

write.csv(mean_3year, "Annual_means_average_2013_2016_PM2_5.csv", row.names=FALSE)

#####

## the 24hr PM 2.5 value of the stations

daily_24_pm <- function (data, site, years_to_cons) {
  
  data <- data %>%
    mutate(date = ymd(Date, tz = "UTC"),
           year = year(date),
           month = month(date)) %>%
    dplyr:: select(date,
                   year,
                   month,
                   Pollutant,
                   Daily_mean,
                   Cap,
                   Site) %>%
    filter(Pollutant == "PM2.5"  & Site == site )
  #### removing all values with data capture less than 75%
 ind_cap_less<- which(data$Cap< 75)
 data$Daily_mean[ind_cap_less]<-NA
  
  ## get the months of observations
  max_daily_pm<- data.frame()
  
  for (kk in 1:length(years_to_cons)) {
    year_of<-as.numeric(years_to_cons[kk])
    
    data_123 <- data %>%
      filter(year==year_of)
   
    "98 percentile" = quantile(data_123$Daily_mean, c(0.98),  na.rm = TRUE)
    "96 percentile" = quantile(data_123$Daily_mean, c(0.96),  na.rm = TRUE)
    "94 percentile" = quantile(data_123$Daily_mean, c(0.94),  na.rm = TRUE)
    "90 percentile" = quantile(data_123$Daily_mean, c(0.90),  na.rm = TRUE)
    "75 percentile" = quantile(data_123$Daily_mean, c(0.75),  na.rm = TRUE)
    "50 percentile" = quantile(data_123$Daily_mean, c(0.50),  na.rm = TRUE)
    "25 percentile" = quantile(data_123$Daily_mean, c(0.25),  na.rm = TRUE)
    
    
    perciltile_ <- cbind(`98 percentile`,`96 percentile`,`94 percentile`,`90 percentile`,`75 percentile`,`50 percentile`,`25 percentile`)
    perciltile_<- cbind(perciltile_, as.data.frame(year_of,col.names = "year" ),  as.data.frame(site, col.names = "Site"))
    rownames(perciltile_) <- c()
    max_daily_pm<- rbind(max_daily_pm, perciltile_)
    
  }
  
  mean_3y<-as.data.frame(lapply(max_daily_pm, mean, na.rm = T))
  mean_3y$site<- site
  last_result<- list(max_daily_pm,mean_3y)
  return(last_result)
}

data<-AQ_data

years_to_cons<- c(2014, 2015, 2016)
names_site<- unique(data$Site)
maximum_daily<- data.frame()
maximum_daily_year<-data.frame()
for (jj in names_site){
  
dawit_max<- daily_24_pm(data, site=jj, years_to_cons)
all_year <- as.data.frame(dawit_max[1])
mean_year<- as.data.frame(dawit_max[2])
maximum_daily<-rbind(maximum_daily,all_year)
maximum_daily_year<-rbind(maximum_daily_year,mean_year)
}


write.csv(maximum_daily, "24hr_PM2_5_years.csv", row.names=FALSE)
write.csv(maximum_daily_year, "24hr_PM2_5.csv", row.names=FALSE)
