hour2day_2<- setNames(aggregate(hour2day$Value, by=list(hour2day$date_re), mean_na), c("Date", "Daily_mean"))




Data_frame_all<-as.data.frame(lapply(Data_frame_all, rep, nrow(hour2day_2)))





# i<-"Hamdan Street" # to be deleted
stat<- filter(raw_sel, Site == i )
pollu <- unique(stat$Pollutant)
#number_out<-data.frame()

for(j in pollu){
  #  j<-"SO2"# to be deleted
  to_walash_ <- filter(DM, Site == i )
  to_walash_ <- filter(to_walash_, Pollutant == j )
  
  aggregate(hour2day$Value, by=list(date_re), mean_na)
  
  
  group_by(date_re) %>%
    mutate(daily_val = mean(Value, na.rm = TRUE))
  #summarise(daily_val= mean(Value, na.rm = TRUE ))
  #ungroup()
  
  
  
  
  mar_date<- unique(hour2day$date_form)
  
  mar<- which(unique(hour2day$date_form))
  
  
  
  
  summarise_all(funs(mean_na))
  
  
  
  
  
  
  
  mutate(year = year(DateTime),
         month = month(DateTime),
         day_num= day(DateTime)) #%>%
  
  ## group them by values which have the same year, month and date
  
  hour2day %>%
    group_by(year, month, day_num) %>%
    
    
    dawit<-hour2day %>%
    group_by(year, month, day_num) %>%
    summarise(mean)
  
  
  to_walash$quarter <- character(length = nrow(to_walash))
  to_walash$quarter[to_walash$month %in% c(1:3)] <- "Q1"
  to_walash$quarter[to_walash$month %in% c(4:6)] <- "Q2"
  to_walash$quarter[to_walash$month %in% c(7:9)] <- "Q3"
  to_walash$quarter[to_walash$month %in% c(10:12)] <- "Q4"
  setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates")
  NCMS_2013 <- read_csv("database_NCMS_2013_hourly.csv")
  NCMS_2014 <- read_csv("database_NCMS_2014_hourly.csv")
  NCMS_2015 <- read_csv("database_NCMS_2015_hourly.csv")
  NCMS_2016 <- read_csv("database_NCMS_2016_hourly.csv")
  
  # bind data together
  NCMS <- rbind(NCMS_2013 ,NCMS_2014) #, NCMS_2015, NCMS_2016)
  
  
  
  
  # to get the number of counts without NaN
  apply(ZZZ, 2, function(x) length(which(!is.na(x))))
  
  # to change the daily to hourly
  NCMS <-NCMS %>%
    mutate(date = ymd(DateTime))
  
  
  ,
  month = month(DateTime)) #%>%


NCMS$Date <- as.Date(NCMS$Date, "%m/%d/%Y")
dawit$DateTime<- floor(dawit$DateTime)
dawit$Pollutant
zzz<-dawit [c("DateTime", "Site", "Pollutant" )]
if (dawit$DateTime )
  ddd<-group_by(dawit, c("DateTime", "Site", "Pollutant" ),add= FALSE)

dawit<-DM_2013


dawit$Value[is.na(dawit$Value)] <- NA
zzz<-dawit
zzz$DateTime<- floor(zzz$DateTime)

floor(zzz$DateTime[25])

ddd_mean<- dawit %>%
  group_by(DateTime, Site,Pollutant) %>%
  summarise(newvar = mean(Value, na.rm = TRUE))
ddd<- dawit %>%
  group_by(DateTime, Site,Pollutant) %>%  
  summarise(newvar2 = length(which(!is.na(Value)))*100/25)

ag <- aggregate(dawit ~ c("DateTime", "Site", "Pollutant" ) , dawit, stat(dawit$Value))