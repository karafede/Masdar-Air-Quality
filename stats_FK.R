

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)

setwd('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_Mean')

# setwd("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data")


###########################################################################
###########################################################################

EAD_data_2013 <- read_csv('database_EAD_2013_daily_mean.csv')
EAD_data_2014 <- read_csv('database_EAD_2014_daily_mean.csv')
EAD_data_2015 <- read_csv('database_EAD_2015_daily_mean.csv')
EAD_data_2016 <- read_csv('database_EAD_2016_daily_mean.csv')
EAD_data_2017 <- read_csv('database_EAD_2017_daily_mean.csv')

DM_data_2013 <- read_csv('database_DM_2013_daily_mean.csv')
DM_data_2014 <- read_csv('database_DM_2014_daily_mean.csv')
DM_data_2015 <- read_csv('database_DM_2015_daily_mean.csv')
DM_data_2016 <- read_csv('database_DM_2016_daily_mean.csv')
DM_data_2017 <- read_csv('database_DM_2017_daily_mean.csv')

NCMS_data_2013 <- read_csv('database_NCMS_2013_daily_mean.csv')
NCMS_data_2014 <- read_csv('database_NCMS_2014_daily_mean.csv')
NCMS_data_2015 <- read_csv('database_NCMS_2015_daily_mean.csv')
NCMS_data_2016 <- read_csv('database_NCMS_2016_daily_mean.csv')
NCMS_data_2017 <- read_csv('database_NCMS_2017_daily_mean.csv')

AQ_data <- rbind(EAD_data_2013, EAD_data_2014, EAD_data_2015, EAD_data_2016, EAD_data_2017,
                 DM_data_2013, DM_data_2014, DM_data_2015, DM_data_2016, DM_data_2017,
                 NCMS_data_2013, NCMS_data_2014, NCMS_data_2015, NCMS_data_2016,NCMS_data_2017)

# replace NaN (not a Number with NA that is a missing value)
AQ_data[sapply(AQ_data,is.na)] = NA 


 # load Ozone data

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_O3/")
EAD_O3_2013 <- read_csv("database_EAD_2013_O3_daily.csv")
EAD_O3_2014 <- read_csv("database_EAD_2014_O3_daily.csv")
EAD_O3_2015 <- read_csv("database_EAD_2015_O3_daily.csv")
EAD_O3_2016 <- read_csv("database_EAD_2016_O3_daily.csv")
EAD_O3_2017 <- read_csv("database_EAD_2017_O3_daily.csv")

DM_O3_2013 <- read_csv("database_DM_2013_O3_daily.csv")
DM_O3_2014 <- read_csv("database_DM_2014_O3_daily.csv")
DM_O3_2015 <- read_csv("database_DM_2015_O3_daily.csv")
DM_O3_2016 <- read_csv("database_DM_2016_O3_daily.csv")
DM_O3_2017 <- read_csv("database_DM_2017_O3_daily.csv")

NCMS_O3_2013 <- read_csv("database_NCMS_2013_O3_daily.csv")
NCMS_O3_2014 <- read_csv("database_NCMS_2014_O3_daily.csv")
NCMS_O3_2015 <- read_csv("database_NCMS_2015_O3_daily.csv")
NCMS_O3_2016 <- read_csv("database_NCMS_2016_O3_daily.csv")
NCMS_O3_2017 <- read_csv("database_NCMS_2017_O3_daily.csv")

O3_data <- rbind(EAD_O3_2013, EAD_O3_2014, EAD_O3_2015, EAD_O3_2016, EAD_O3_2017,
                 DM_O3_2013, DM_O3_2014, DM_O3_2015, DM_O3_2016, DM_O3_2017,
                 NCMS_O3_2013, NCMS_O3_2014, NCMS_O3_2015, NCMS_O3_2016, NCMS_O3_2017)


# replace NaN (not a Number with NA that is a missing value)
O3_data[sapply(O3_data,is.na)] = NA 

O3_data$Mean_8hour <- as.numeric(O3_data$Mean_8hour)
O3_data$MAX_8hour <- as.numeric(O3_data$MAX_8hour)


O3_data <- O3_data %>%
  mutate(year = year(Date)) %>%
  dplyr:: select(Date,
                 Site,
                 MAX_8hour,
                 year,
                 Capture)
str(O3_data)


# load CO data
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Daily_CO")

EAD_CO_2013 <- read_csv("database_EAD_2013_CO_daily.csv")
EAD_CO_2014 <- read_csv("database_EAD_2014_CO_daily.csv")
EAD_CO_2015 <- read_csv("database_EAD_2015_CO_daily.csv")
EAD_CO_2016 <- read_csv("database_EAD_2016_CO_daily.csv")
EAD_CO_2017 <- read_csv("database_EAD_2017_CO_daily.csv")

DM_CO_2013 <- read_csv("database_DM_2013_CO_daily.csv")
DM_CO_2014 <- read_csv("database_DM_2014_CO_daily.csv")
DM_CO_2015 <- read_csv("database_DM_2015_CO_daily.csv")
DM_CO_2016 <- read_csv("database_DM_2016_CO_daily.csv")
DM_CO_2017 <- read_csv("database_DM_2017_CO_daily.csv")

NCMS_CO_2013 <- read_csv("database_NCMS_2013_CO_daily.csv")
NCMS_CO_2014 <- read_csv("database_NCMS_2014_CO_daily.csv")
NCMS_CO_2015 <- read_csv("database_NCMS_2015_CO_daily.csv")
NCMS_CO_2016 <- read_csv("database_NCMS_2016_CO_daily.csv")
NCMS_CO_2017 <- read_csv("database_NCMS_2017_CO_daily.csv")

CO_data <- rbind(EAD_CO_2013, EAD_CO_2014, EAD_CO_2015, EAD_CO_2016, EAD_CO_2017,
                 DM_CO_2013, DM_CO_2014, DM_CO_2015, DM_CO_2016, DM_CO_2017,
                 NCMS_CO_2013, NCMS_CO_2014, NCMS_CO_2015, NCMS_CO_2016, NCMS_CO_2017)

# replace NaN (not a Number with NA that is a missing value)
CO_data[sapply(CO_data,is.na)] = NA 

CO_data$Mean_8hour <- as.numeric(CO_data$Mean_8hour)
CO_data$MAX_8hour <- as.numeric(CO_data$MAX_8hour)


CO_data <- CO_data %>%
  mutate(year = year(Date)) %>%
  dplyr:: select(Date,
                 Site,
                 MAX_8hour,
                 year,
                 Capture)

##-------------------------------------------------------------------
# boxplots----#######################################################

## PM10----------------------------------------------------------------

colnames(AQ_data)[10] <- "Cap"
colnames(AQ_data)[12] <- "Value"

AQ_data_PM10 <- AQ_data %>%
 mutate(date = ymd(Date, tz = "UTC"),
        year = year(Date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year,
                 Cap) %>%
  filter(Cap > 75) %>%
  filter(Pollutant == "PM10")


# make a box plot with ggplot----------------------------------------

plot <- ggplot(AQ_data_PM10, aes(Site, Value, fill = Site)) +
  theme_bw() +
 geom_boxplot() + 
#  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 200) +
  theme( strip.text = element_text(size = 18)) + 
  xlab("Site") +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="bold")) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour = "black")) +
  geom_hline(yintercept=150, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of 24h-averaged"," ", PM[10], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) 
  
plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/PM10_boxplot.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



# ### Data Capture PM10 ###########
# 
# jpeg('summary_plots/PM10_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(AQ_data_PM10, aes(Site, Cap, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   #  facet_grid(. ~ year) +
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle(expression(paste("Data Capture"," ","(", PM[10],")"))) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()


### Annual mean PM10 concentration #################################


AQ_data_PM10_AVG <- AQ_data_PM10 %>%
  group_by(year,
           Site) %>%
  filter(Value > 0) %>%
  filter(Value < 200) %>%
  summarise(AVG_Value = mean(Value))

# Annual Mean UAE-----------------------
Annual_Mean_PM10 <- AQ_data_PM10 %>%
  group_by(year) %>%
  filter(Value > 0) %>%
  filter(Value < 200) %>%
  filter(Cap > 75) %>%
  summarise(mean_PM10 = mean(Value))

Annual_Mean_PM10$year <- as.factor(Annual_Mean_PM10$year)
AQ_data_PM10_AVG$year <- as.factor(AQ_data_PM10_AVG$year)

write_csv(Annual_Mean_PM10, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Annual_Mean_PM10.csv")


plot <- ggplot(AQ_data_PM10_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 150) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
  geom_hline(yintercept=40, col="red", size = 1) +
  geom_hline(yintercept=150, col="blue", size = 1) +
  ggtitle(expression(paste("Annual distribution of daily"," ", PM[10]," concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  geom_text(aes(x = 1.05 , y = 35, label = "EU annual limit value"), size = 7) +
  geom_text(aes(x = 1 , y = 145, label = "UAE 24h limit value"), size = 7) +
  
  geom_point(data = Annual_Mean_PM10, aes(year,mean_PM10), 
             color='black', size = 8, shape=18)
  
plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/PM10_annual.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


######################################################################
######################################################################

## PM2.5----------------------------------------------------------------


AQ_data_PM25 <- AQ_data %>%
  mutate(date = ymd(Date, tz = "UTC"),
         year = year(Date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year,
                 Cap) %>%
  filter(Cap > 75) %>%
  filter(Pollutant == "PM2.5")


plot <- ggplot(AQ_data_PM25, aes(Site, Value, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 150) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="bold")) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour = "black")) +
  geom_hline(yintercept=35, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of 24h-averaged"," ", PM[2.5], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/PM2.5_boxplot.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


# ### Data Capture PM2.5 ###########
# 
# jpeg('summary_plots/PM25_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(AQ_data_PM25, aes(Site, Cap, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   #  facet_grid(. ~ year) +
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle(expression(paste("Data Capture"," ","(", PM[2.5],")"))) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()



### Annual mean PM2.5 concentration #################################


AQ_data_PM25_AVG <- AQ_data_PM25 %>%
  group_by(year,
           Site) %>%
  filter(Value > 0) %>%
  filter(Value < 200) %>%
  filter(Cap > 75) %>%
  summarise(AVG_Value = mean(Value))

# Annual Mean UAE-----------------------
# Annual_Mean_PM25 <- read_csv("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Annual_means_Quarters_PM2.5_new.csv")
Annual_Mean_PM25 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Annual_means_Quarters_PM2_5_new.csv")
                                                                                                               

Annual_Mean_PM25 <- Annual_Mean_PM25 %>%
  group_by(year) %>%
    summarise(mean_PM25 = mean(annual_AVG, na.rm=TRUE))

write_csv(Annual_Mean_PM25, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Annual_Mean_PM25.csv")


# Annual Mean UAE-----------------------
# Annual_Mean_PM25 <- AQ_data_PM25 %>%
#   group_by(year) %>%
#   filter(Value > 0) %>%
#   filter(Value < 200) %>%
#   summarise(mean_PM25 = mean(Value))

AQ_data_PM25_AVG$year <- as.factor(AQ_data_PM25_AVG$year)
Annual_Mean_PM25$year <- as.factor(Annual_Mean_PM25$year)


plot <- ggplot(AQ_data_PM25_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 75) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
  geom_hline(yintercept=15, col="blue", size = 1) +
  geom_hline(yintercept=35, col="red", size = 1) +
  ggtitle(expression(paste("Annual distribution of daily"," ", PM[2.5]," concentration"))) +  
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  # geom_text(aes(x = 0.75 , y = 23, label = "EU limit value"), size = 7) +
  # geom_hline(yintercept=25, col="red", size = 1) +
  
  geom_text(aes(x = 0.97 , y = 38, label = "EPA 24h limit value"), size = 7) +
  geom_text(aes(x = 1.06 , y = 18, label = "EPA annual limit value"), size = 7) +

  
  geom_point(data = Annual_Mean_PM25, aes(year,mean_PM25), 
             color='black', size = 8, shape=18)

plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/PM2.5_annual.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



######################################################################
######################################################################

# SO2 and NO2 (1-hr)---------------------------------------------------------------------
# for EAD use filtered data (4 boxplot)
setwd('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/Filtered_4_Box')
# dir_SO2_NO2 <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box"

EAD_SO2_NO2_2013 <- read_csv("database_EAD_ 2013 _hourly_filtered.csv")
EAD_SO2_NO2_2014 <- read_csv("database_EAD_ 2014 _hourly_filtered.csv")
EAD_SO2_NO2_2015 <- read_csv("database_EAD_ 2015 _hourly_filtered.csv")
EAD_SO2_NO2_2016 <- read_csv("database_EAD_ 2016 _hourly_filtered.csv")
EAD_SO2_NO2_2017 <- read_csv("database_EAD_ 2017 _hourly_filtered.csv")

DM_SO2_NO2_2013 <- read_csv("database_DM_ 2013 _hourly_filtered.csv")
DM_SO2_NO2_2014 <- read_csv("database_DM_ 2014 _hourly_filtered.csv")
DM_SO2_NO2_2015 <- read_csv("database_DM_ 2015 _hourly_filtered.csv")
DM_SO2_NO2_2016 <- read_csv("database_DM_ 2016 _hourly_filtered.csv")
DM_SO2_NO2_2017 <- read_csv("database_DM_ 2017 _hourly_filtered.csv")

NCMS_SO2_NO2_2013 <- read_csv("database_NCMS_ 2013 _hourly_filtered.csv")
NCMS_SO2_NO2_2014 <- read_csv("database_NCMS_ 2014 _hourly_filtered.csv")
NCMS_SO2_NO2_2015 <- read_csv("database_NCMS_ 2015 _hourly_filtered.csv")
NCMS_SO2_NO2_2016 <- read_csv("database_NCMS_ 2016 _hourly_filtered.csv")
NCMS_SO2_NO2_2017 <- read_csv("database_NCMS_ 2017 _hourly_filtered.csv")

SO2_NO2_all <- rbind(EAD_SO2_NO2_2013, EAD_SO2_NO2_2014, EAD_SO2_NO2_2015, EAD_SO2_NO2_2016, EAD_SO2_NO2_2017,
                     DM_SO2_NO2_2013, DM_SO2_NO2_2014, DM_SO2_NO2_2015, DM_SO2_NO2_2016, DM_SO2_NO2_2017,
                     NCMS_SO2_NO2_2013, NCMS_SO2_NO2_2014, NCMS_SO2_NO2_2015, NCMS_SO2_NO2_2016, NCMS_SO2_NO2_2017)

SO2_NO2_all <- SO2_NO2_all %>%
  select(DateTime,
         Site,
         Pollutant,
         Site_Type,
         Latitude,
         Longitude,
         Value) 


## NO2----------------------------------------------------------------



AQ_data_NO2 <- SO2_NO2_all %>%
  mutate(date = ymd_hms(DateTime, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year) %>%
  filter(Pollutant == "NO2")



plot <- ggplot(AQ_data_NO2, aes(Site, Value, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 200) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="bold")) +
  ylab(expression(paste(NO[2], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour = "black")) +
#  geom_hline(yintercept= 400, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of hourly"," ", NO[2], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/NO2_boxplot.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()




### Data Capture NO2 ###########

# jpeg('summary_plots/NO2_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(AQ_data_NO2, aes(Site, Cap, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   #  facet_grid(. ~ year) +
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle(expression(paste("Data Capture"," ","(", NO[2],")"))) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()


### Annual mean NO2 concentration #################################


AQ_data_NO2_AVG <- AQ_data_NO2 %>%
  group_by(year,
           Site) %>%
   filter(Value > 0) %>%
  # filter(Value < 200) %>%
  summarise(AVG_Value = mean(Value))


# Annual Mean UAE-----------------------
Annual_Mean_NO2 <- AQ_data_NO2 %>%
  group_by(year) %>%
  filter(Value > 0) %>%
  summarise(mean_NO2 = mean(Value))

write_csv(Annual_Mean_NO2, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Annual_Mean_NO2.csv")


Annual_Mean_NO2$year <- as.factor(Annual_Mean_NO2$year)
AQ_data_NO2_AVG$year <- as.factor(AQ_data_NO2_AVG$year)


plot <- ggplot(AQ_data_NO2_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 80) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(NO[2], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
  geom_hline(yintercept=40, col="red", size = 1) +
  ggtitle(expression(paste("Annual distribution of hourly"," ", NO[2]," concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  geom_text(aes(x = 0.8 , y = 44, label = "EU limit value"), size = 7) +
  
  geom_point(data = Annual_Mean_NO2, aes(year,mean_NO2), 
             color='black', size = 8, shape=18)
plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/NO2_annual.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



######################################################################
######################################################################

## SO2----------------------------------------------------------------


AQ_data_SO2 <- SO2_NO2_all %>%
  mutate(date = ymd_hms(DateTime, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year) %>%
  filter(Pollutant == "SO2")




plot <- ggplot(AQ_data_SO2, aes(Site, Value, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 200) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="bold")) +
  ylab(expression(paste(SO[2], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour = "black")) +
#  geom_hline(yintercept=197, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of hourly"," ", SO[2], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
  

plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/SO2_boxplot.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()






### Data Capture SO2 ###########

# jpeg('summary_plots/SO2_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(AQ_data_SO2, aes(Site, Cap, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   #  facet_grid(. ~ year) +
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle(expression(paste("Data Capture"," ","(", SO[2],")"))) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()
# 


### Annual mean SO2 concentration #################################


AQ_data_SO2 <- AQ_data %>%
  mutate(date = ymd(Date, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year,
                 Cap) %>%
  filter(Pollutant == "SO2")


AQ_data_SO2_AVG <- AQ_data_SO2 %>%
  group_by(year,
           Site) %>%
  filter(Value > 0) %>%
  # filter(Value < 200) %>%
  summarise(AVG_Value = mean(Value))

# Annual Mean UAE-----------------------
Annual_Mean_SO2 <- AQ_data_SO2 %>%
  group_by(year) %>%
  filter(Value > 0) %>%
  summarise(mean_SO2 = mean(Value))

Annual_Mean_SO2$year <- as.factor(Annual_Mean_SO2$year)
AQ_data_SO2_AVG$year <- as.factor(AQ_data_SO2_AVG$year)



write_csv(Annual_Mean_SO2, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Annual_Mean_SO2.csv")



plot <- ggplot(AQ_data_SO2_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 20) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(SO[2], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
  geom_hline(yintercept=125, col="red", size = 1) +
  ggtitle(expression(paste("Annual distribution of daily"," ", SO[2]," concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  #  geom_text(aes(x = 0.75 , y = 37, label = "EU limit value"), size = 7) +
  
  geom_point(data = Annual_Mean_SO2, aes(year,mean_SO2), 
             color='black', size = 8, shape=18)

plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/SO2_annual.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



######################################################################
######################################################################

## CO----------------------------------------------------------------


plot <- ggplot(CO_data, aes(Site, MAX_8hour, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 3) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="bold")) +
  ylab(expression(paste("CO", " (mg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour = "black")) +
  geom_hline(yintercept=125, col="red", size = 1) +
  ggtitle("Distribution of Daily Maximum 8-hour mean CO concentration") +  
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/CO_boxplot.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



### Data Capture CO ###########

# jpeg('summary_plots/CO_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(CO_data, aes(Site, Capture, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle("Data Capture CO") + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()
# 


### Annual mean CO concentration #################################


AQ_data_CO_AVG <- CO_data %>%
  group_by(year,
           Site) %>%
  filter(MAX_8hour > 0) %>%
   filter(MAX_8hour < 100) %>%
  summarise(AVG_Value = mean(MAX_8hour))


# Annual Mean UAE-----------------------
Annual_Mean_CO <- CO_data %>%
  group_by(year) %>%
  filter(MAX_8hour > 0) %>%
  filter(MAX_8hour < 10) %>%
  summarise(mean_CO = mean(MAX_8hour))

write_csv(Annual_Mean_CO, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Annual_Mean_CO_Max_8h.csv")


Annual_Mean_CO$year <- as.factor(Annual_Mean_CO$year)
AQ_data_CO_AVG$year <- as.factor(AQ_data_CO_AVG$year)


plot <- ggplot(AQ_data_CO_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 2.5) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste("CO", " (mg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
 # geom_hline(yintercept=125, col="red", size = 1) +
  ggtitle("Annual distribution of maximum 8h-mean CO concentration") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  geom_point(data = Annual_Mean_CO, aes(year,mean_CO), 
             color='black', size = 8, shape=18)

plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/CO_annual.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


############################################################################
############################################################################


# OZONE--------------------------------------------------------------------


# load Valid Daily Max according to US-EPA regulations
Daily_Max_O3 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_4_values_o3_transposed.csv")
# Daily_Max_O3 <- read_csv("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_O3/MAX_4_values_o3_transposed.csv")
Daily_Max_O3$year <- as.factor(Daily_Max_O3$year)
Daily_Max_O3$sorted_max_by_year <- Daily_Max_O3$sorted_max_by_year *1960



plot <- ggplot(O3_data, aes(Site, MAX_8hour, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 200) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="bold")) +
  ylab(expression(paste(O[3], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour = "black")) +
  geom_hline(yintercept=120, col="red", size = 1) +
  ggtitle(expression(paste("Distribution of Daily Maximum 8-hour mean"," ", O[3], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
  
  
plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/O3_boxplot.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()

#################################################
# sorted highest daily max 8-hour concentration ########



# load Valid Daily Max according to US-EPA regulations

# run the code "Z:\_SHARED_FOLDERS\Air Quality\Phase 1\Pathflow of Phase I_DG\dawit Data\daily data\Daily_O3\max_o3_regulation_EPA.R"
# transpose all data in "MAX_4_values_o3.csv" and generate "MAX_4_values_o3_transposed.csv"

Daily_Max_O3 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_4_values_o3_transposed.csv")
# Daily_Max_O3 <- read_csv("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_O3/MAX_4_values_o3_transposed.csv")
Daily_Max_O3$year <- as.factor(Daily_Max_O3$year)
Daily_Max_O3$sorted_max_by_year <- Daily_Max_O3$sorted_max_by_year *1960

EPA_4th_highest_daily <- 1960 * 0.08


plot <- ggplot(Daily_Max_O3, aes(Site, sorted_max_by_year, fill = Site)) +
  theme_bw() +
  geom_point(color="blue", size = 6, shape=17) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 300) +
  xlab("Site") +
  theme( strip.text = element_text(size = 18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=16, colour = "black", face="bold")) +
  ylab(expression(paste(O[3], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=16),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=12, colour = "black")) +
  geom_hline(yintercept=156.8, col="red", size = 1) +
  ggtitle(expression(paste("4th highest Daily Maximum 8-hour average"," ", O[3], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5)) 
  
  
  plot

  
  png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/O3_highest_Daily_Max_boxplot.jpg",
      width = 1800, height = 1050, units = "px", pointsize = 30,
      bg = "white", res = 150)
  print(plot)
  dev.off()



### Data Capture O3 ###########

# jpeg('summary_plots/O3_capture.jpg',
#      quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# plot <- ggplot(O3_data, aes(Site, Capture, fill = Site)) +
#   theme_bw() +
#   geom_boxplot() + 
#   facet_grid(year ~ .) +
#   guides(fill=FALSE) +   # no legend
#   ylim(0, 100) +
#   theme( strip.text = element_text(size = 18)) + 
#   xlab("Site") +
#   ylab(expression("Data Capture (%)")) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=18, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=18),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=16, colour = "black")) +
#   geom_hline(yintercept=75, col="red", size = 1) +
#   ggtitle(expression(paste("Data Capture"," ","(", O[3],")"))) + 
#   theme(plot.title = element_text(lineheight=.8, face="bold", size = 20, hjust = 0.5))
# plot
# 
# 
# par(oldpar)
# dev.off()



### Annual mean O3 concentration #################################


AQ_data_O3_AVG <- O3_data %>%
  group_by(year,
           Site) %>%
  filter(MAX_8hour > 0) %>%
  summarise(AVG_Value = mean(MAX_8hour))


# load Valid Daily Max according to US-EPA regulations
# Daily_Max_O3 <- read_csv("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/Daily_O3/MAX_4_values_o3_transposed.csv")
Daily_Max_O3 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/MAX_4_values_o3_transposed.csv")

Daily_Max_O3 <- Daily_Max_O3 %>%
  group_by(year) %>%
  summarise(mean_O3 = mean(sorted_max_by_year, na.rm=TRUE))
Daily_Max_O3$mean_O3 <- Daily_Max_O3$mean_O3 *1960

write_csv(Daily_Max_O3, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/Daily_Max_O3.csv")


Daily_Max_O3$year <- as.factor(Daily_Max_O3$year)
AQ_data_O3_AVG$year <- as.factor(AQ_data_O3_AVG$year)



plot <- ggplot(AQ_data_O3_AVG, aes(year, AVG_Value, fill = year)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(0, 200) +
  guides(fill=FALSE) +   # no legend
  ylab(expression(paste(O[3], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=24, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=24),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=24, colour = "black")) +
#  geom_hline(yintercept=156.8, col="blue", size = 1,linetype="dashed") +
  geom_hline(yintercept=120, col="red", size = 1) +
  ggtitle(expression(paste("Annual distribution of maximum 8h-mean"," ", O[3]," concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 25, hjust = 0.5)) +
  
  geom_text(aes(x = 1.25 , y = 127, label = "UAE 8h-averaged limit value"), size = 7) +
  
  geom_point(data = Daily_Max_O3, aes(year,mean_O3), 
             color='blue', size = 8, shape=18)

plot


png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/AQ_TIDY_Data/OUTPUT_DB_Data/AQ_Stats/O3_annual.jpg",
    width = 1800, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()