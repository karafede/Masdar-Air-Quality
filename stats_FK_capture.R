

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data/")
setwd("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/daily data")

###########################################################################
###########################################################################


EAD_data_2013 <- read_csv("database_EAD_2013_daily.csv")
EAD_data_2014 <- read_csv("database_EAD_2014_daily.csv")
EAD_data_2015 <- read_csv("database_EAD_2015_daily.csv")
EAD_data_2016 <- read_csv("database_EAD_2016_daily.csv")

DM_data_2013 <- read_csv("database_DM_2013_daily.csv")
DM_data_2014 <- read_csv("database_DM_2014_daily.csv")
DM_data_2015 <- read_csv("database_DM_2015_daily.csv")
DM_data_2016 <- read_csv("database_DM_2016_daily.csv")

NCMS_data_2013 <- read_csv("database_NCMS_2013_daily.csv")
NCMS_data_2014 <- read_csv("database_NCMS_2014_daily.csv")
NCMS_data_2015 <- read_csv("database_NCMS_2015_daily.csv")
NCMS_data_2016 <- read_csv("database_NCMS_2016_daily.csv")

AQ_data <- rbind(EAD_data_2013, EAD_data_2014, EAD_data_2015, EAD_data_2016, 
                 DM_data_2013, DM_data_2014, DM_data_2015, DM_data_2016,
                 NCMS_data_2013, NCMS_data_2014, NCMS_data_2015, NCMS_data_2016)

# replace NaN (not a Number with NA that is a missing value)
AQ_data[sapply(AQ_data,is.na)] = NA 


 # load Ozone data

wd <- getwd()
EAD_O3_2013 <- read_csv(paste0(wd,"/Daily_O3/database_EAD_2013_O3_daily.csv"))
EAD_O3_2014 <- read_csv(paste0(wd,"/Daily_O3/database_EAD_2014_O3_daily.csv"))
EAD_O3_2015 <- read_csv(paste0(wd,"/Daily_O3/database_EAD_2015_O3_daily.csv"))
EAD_O3_2016 <- read_csv(paste0(wd,"/Daily_O3/database_EAD_2016_O3_daily.csv"))

DM_O3_2013 <- read_csv(paste0(wd,"/Daily_O3/database_DM_2013_O3_daily.csv"))
DM_O3_2014 <- read_csv(paste0(wd,"/Daily_O3/database_DM_2014_O3_daily.csv"))
DM_O3_2015 <- read_csv(paste0(wd,"/Daily_O3/database_DM_2015_O3_daily.csv"))
DM_O3_2016 <- read_csv(paste0(wd,"/Daily_O3/database_DM_2016_O3_daily.csv"))

NCMS_O3_2013 <- read_csv(paste0(wd,"/Daily_O3/database_NCMS_2013_O3_daily.csv"))
NCMS_O3_2014 <- read_csv(paste0(wd,"/Daily_O3/database_NCMS_2014_O3_daily.csv"))
NCMS_O3_2015 <- read_csv(paste0(wd,"/Daily_O3/database_NCMS_2015_O3_daily.csv"))
NCMS_O3_2016 <- read_csv(paste0(wd,"/Daily_O3/database_NCMS_2016_O3_daily.csv"))

O3_data <- rbind(EAD_O3_2013, EAD_O3_2014, EAD_O3_2015, EAD_O3_2016,
                 DM_O3_2013, DM_O3_2014, DM_O3_2015, DM_O3_2016,
                 NCMS_O3_2013, NCMS_O3_2014, NCMS_O3_2015, NCMS_O3_2016)


# replace NaN (not a Number with NA that is a missing value)
O3_data[sapply(O3_data,is.na)] = NA 

O3_data$Mean_8hour <- as.numeric(O3_data$Mean_8hour)
O3_data$MAX_8hour <- as.numeric(O3_data$MAX_8hour)


O3_data <- O3_data %>%
  mutate(year = year(Date)) %>%
  dplyr:: select(Date,
                 Site,
                 MAX_8hour,
                 year)
str(O3_data)


# AQ_data <- AQ_data %>%
#   mutate(date = mdy(date, tz = "UTC")) %>%
#   dplyr:: select(date,
#                  Site,
#                  Pollutant,
#                  Value) %>%
#   filter(Site == "Al Hamriyah")


##-------------------------------------------------------------------
# boxplots----#######################################################


AQ_data_PM10 <- AQ_data %>%
 mutate(date = mdy(date, tz = "UTC"),
        year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year,
                 Cap) %>%
  filter(Pollutant == "PM10")


# make a box plot with ggplot----------------------------------------


jpeg('summary_plots/PM10_boxplot.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_PM10, aes(Site, Cap, fill = Site)) +
  theme_bw() +
 geom_boxplot() + 
#  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 100) +
  xlab("Site") +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
  geom_hline(yintercept=50, col="red") +
  ggtitle(expression(paste("Distribution of 24h-averaged"," ", PM[10], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 14, hjust = 0.5))
plot


par(oldpar)
dev.off()

######################################################################
######################################################################


AQ_data_PM25 <- AQ_data %>%
  mutate(date = mdy(date, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year) %>%
  filter(Pollutant == "PM2.5")



jpeg('summary_plots/PM2.5_boxplot.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_PM25, aes(Site, Value, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 150) +
  xlab("Site") +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
  geom_hline(yintercept=25, col="red") +
  ggtitle(expression(paste("Distribution of 24h-averaged"," ", PM[2.5], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 14, hjust = 0.5))
plot


par(oldpar)
dev.off()



######################################################################
######################################################################


AQ_data_NO2 <- AQ_data %>%
  mutate(date = mdy(date, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year) %>%
  filter(Pollutant == "NO2")



jpeg('summary_plots/NO2_boxplot.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_NO2, aes(Site, Value, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 120) +
  xlab("Site") +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(NO[2], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
  geom_hline(yintercept=40, col="red") +
  ggtitle(expression(paste("Summary distribution of daily"," ", NO[2], " concentration by site and by year"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 14, hjust = 0.5))
plot


par(oldpar)
dev.off()


######################################################################
######################################################################


AQ_data_SO2 <- AQ_data %>%
  mutate(date = mdy(date, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year) %>%
  filter(Pollutant == "SO2")



jpeg('summary_plots/SO2_boxplot.jpg',
     quality = 100, bg = "white", res = 600, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_SO2, aes(Site, Value, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 200) +
  xlab("Site") +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(SO[2], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
  geom_hline(yintercept=125, col="red") +
  ggtitle(expression(paste("Distribution of 24h-averaged"," ", SO[2], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 14, hjust = 0.5))
plot


par(oldpar)
dev.off()


######################################################################
######################################################################


AQ_data_CO <- AQ_data %>%
  mutate(date = mdy(date, tz = "UTC"),
         year = year(date)) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value,
                 year) %>%
  filter(Pollutant == "CO")



jpeg('summary_plots/CO_boxplot.jpg',
     quality = 100, bg = "white", res = 600, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_data_CO, aes(Site, Value, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 3) +
  xlab("Site") +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste("CO", " (mg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
  geom_hline(yintercept=10, col="red") +
  ggtitle(expression(paste("Distribution of 24h-averaged"," ", "CO", " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 14, hjust = 0.5))
plot


par(oldpar)
dev.off()


############################################################################
############################################################################


# OZONE

jpeg('summary_plots/O3_boxplot.jpg',
     quality = 100, bg = "white", res = 600, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(O3_data, aes(Site, MAX_8hour, fill = Site)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(. ~ year) +
  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 200) +
  xlab("Site") +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(O[3], " (µg/",m^3, ")"))) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
  geom_hline(yintercept=120, col="red") +
  ggtitle(expression(paste("Distribution of Maximum day 8-hour mean"," ", O[3], " concentration"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 14, hjust = 0.5))
plot


par(oldpar)
dev.off()




######################################################################
######################################################################




######################################################################
######################################################################
######################################################################
######################################################################



######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################



NCMS_data_percentiles <- NCMS_data %>%
  dplyr::group_by(Pollutant) %>%
  summarise("98 percentile" = quantile(Value, c(0.98),  na.rm = TRUE),
            "97 percentile" = quantile(Value, c(0.97),  na.rm = TRUE),
            "96 percentile" = quantile(Value, c(0.96),  na.rm = TRUE),
            "95 percentile" = quantile(Value, c(0.95),  na.rm = TRUE))

#####################################################################
### counting number of observations

EAD_data <- read_csv("database_EAD_2015_daily.csv")

# replace NaN (not a Number with NA that is a missing value)
EAD_data[sapply(EAD_data,is.na)] = NA 
# EAD_data <- na.omit(EAD_data)

EAD_data <- EAD_data %>%
  mutate(date = mdy(date, tz = "UTC")) %>%
  dplyr:: select(date,
                 Site,
                 Pollutant,
                 Value) %>%
  filter(Pollutant == "PM2.5")
 
EAD_data <- EAD_data %>%
  dplyr::group_by(Site) %>%
  summarise("N_above_35ug" = sum(Value > 150, na.rm = TRUE),
            "min" = min(Value, na.rm = TRUE),
            "max" = min(Value, na.rm = TRUE))

# round the values to the nearest integer
EAD_data$min <- round(EAD_data$min, digits = 0)
EAD_data$max <- round(EAD_data$max, digits = 0)

# AAA <- EAD_data$Value > 40
# sum(AAA == TRUE)



##################################################################################
##################################################################################

####################################################################
###########---------------------------------------------------------
## calculation of the annual mean using the EPA protocol


EAD_data <- read_csv("database_EAD_2016_daily.csv")

# get the name of the sites
names <- EAD_data %>% 
  select(Site,
         Value) %>%
  group_by(Site) %>%
  summarise(mean = mean(Value))

list_names <- as.list(names$Site)

# replace NaN (not a Number with NA that is a missing value)
EAD_data[sapply(EAD_data,is.na)] = NA 
# EAD_data <- na.omit(EAD_data)

# percentile_EAD_data <- EAD_data %>%
#   dplyr::group_by(Pollutant,
#                   Site) %>%
#   summarise("98 percentile" = quantile(Value, c(0.98),  na.rm = TRUE),
#             "97 percentile" = quantile(Value, c(0.97),  na.rm = TRUE),
#             "96 percentile" = quantile(Value, c(0.96),  na.rm = TRUE),
#             "95 percentile" = quantile(Value, c(0.95),  na.rm = TRUE))


###############################################################################
# function to all quarterly averages------------------------------------------

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
  
  ## year variable
  data$year <- factor(format(data$date, format = "%Y"))
  
  # make averages by quarters------------------------------------------
  AVG_quarter <- data %>%
    group_by(Site,
             quarter,
             year) %>%
    summarise(mean = mean(Value, na.rm = TRUE))
  
  # return
  AVG_quarter <- as.data.frame(AVG_quarter)
  AVG_quarter
  
}


# loop all the stations and concatenate all the data
All_quarters <- data.frame()
for (i in 1:length(list_names)) {
  All_AVG <- quarterly_averages(EAD_data, unlist(list_names[6]))
  All_AVG <- quarterly_averages(EAD_data, unlist(list_names[i]))
  All_quarters <- rbind(All_quarters, All_AVG)
}



# spread data
All_quarters <- All_quarters %>%
  spread(quarter, mean)


All_quarters$annual_AVG <- rowMeans(All_quarters[ ,2:5])


# end












###################################################################################
###################################################################################
# old and alternative stuff #######################################################

###################################################################################
# function to make and aggregate quarterly averages--------------------------------

annual_averages <- function (data, site) {
  
  data <- data %>%
    mutate(date = mdy(date, tz = "UTC"),
           year = year(date),
           month = month(date)) %>%
    dplyr:: select(date,
                   year,
                   month,
                   Pollutant,
                   Value,
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
  
  ## year variable
  data$year <- factor(format(data$date, format = "%Y"))
  
  # make averages by quarters------------------------------------------
  AVG_quarter <- data %>%
    group_by(Site,
             quarter) %>%
    summarise(mean = mean(Value, na.rm = TRUE))
  
  
  ## and aggregate for each quarter------------------------------------
  
  AVG_QUARTERS <- with(data[,], aggregate(Value, list(quarter = quarter,
                                                      year = year), FUN = mean, na.rm = TRUE))
  
  
  
  names(AVG_QUARTERS)[names(AVG_QUARTERS) == 'x'] <- 'Quarters'
  
  
  # write.csv(AVG_QUARTERS, paste(site,"_quarterly_AVG.csv", sep = ""), row.names=FALSE)
  Annual_mean <- mean(AVG_QUARTERS$Quarters)
  Annual_Average <- data.frame(c("Site", "year", "Pollutant", "Annual Mean"),
                               c(site, 2015, "PM2.5",Annual_mean))
  
  Annual_Average <- as.data.frame (t(Annual_Average))
  colnames(Annual_Average) <- as.character(unlist(Annual_Average[1,]))
  Annual_Average = Annual_Average[-1, ]
  row.names(Annual_Average) <- NULL
  
  # return dataframe
  Annual_Average
  
}


# loop all the stations and concatenate all the data
All_means <- data.frame()
for (i in 1:length(list_names)) {
  # summary_quarter <- quarterly_averages(EAD_data, "Al Ain Islamic Ins")
  summary_quarter <- annual_averages(EAD_data, unlist(list_names[i]))
  All_means <- rbind(All_means, summary_quarter)
}


#######################################################################################
#######################################################################################


