

# Set-up ---------------------------
# Load packages
library(threadr)
library(dygraphs)
library(tidyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/ICS_chem_PM")

####################################################################################
# plot Mass concentrations MASDARD ROOF ############################################
####################################################################################

expo2020 <- read.csv("EXPO2020_PMF.csv")
n_pollutants <- ncol(expo2020)-1

#gather data
expo2020 <- gather(expo2020, "pollutant", "Date")
colnames(expo2020) <- c("Date", "pollutant", "conc")

str(expo2020)

expo2020 <- expo2020 %>%
  mutate(Date = mdy_hm(Date))

##################################################################
##################################################################

mass <- read.csv("Mass_conc_EXPO2020.csv")
str(mass)
mass <- mass[, 1:2]

mass$Date <- mdy_hm(mass$Date)
str(mass)

# repeat mass values as many times the number of pollutants (n_pollutants)
mass_expo2020 <- as.data.frame(rep(mass$PM2.5, n_pollutants))

# bind mass to chemical analysis
expo2020 <- cbind(expo2020, mass_expo2020)
colnames(expo2020) <- c("Date", "pollutant", "conc", "PM2.5")

STATS_EXPO2020 <- mass %>%
  summarise(MEAN_expo2020 = mean(PM2.5, na.rm = TRUE))

###########
## plot ###
###########

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/ICS_chem_PM/EXPO2020/"

min <- as.POSIXct("2018-04-24 08:00:00") 
max <- as.POSIXct("2018-05-28 08:00:00") 

plot <- ggplot(mass, aes(Date, value)) + 
  theme_bw() +
  geom_line(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red", size = 1) +
  geom_point(aes(y = PM2.5, col = "PM2.5"), alpha=1, col="red", size = 4) +
  # geom_line(aes(y = field, col = "field"), col="blue", size = 3) +
 # facet_wrap( ~ station) +
#  theme(legend.position="none") + 
 # theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")", " monitored (24h)"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=15, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=22, colour = "black")) +
#  scale_colour_manual(values=c("red","green","blue")) +
  ylim(0, 500) +
 # xlim(min, max) +
  geom_hline(yintercept = 50, size = 1, linetype = "dashed") +
  scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%Y/%m/%a")) 
plot



png(paste0(output_folder,"time_series_PM2.5_conc_EXPO2020.jpg"),
    width = 1700, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



######################################################################
# load EXPO2020 data #################################################
######################################################################

# expo2020 <- expo2020 %>% 
#   mutate(Date = date(Date))

 str(expo2020)

# calculate midpoints of bars
#  expo2020 <- ddply(expo2020, .(pollutant), 
#                        transform, pos = cumsum(conc) - (0.5 * conc)
# )

# make negative values == Na
# expo2020[expo2020 < 0] <- NA


# stacked plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/ICS_chem_PM/EXPO2020/"

 
 expo2020$Date <- as.POSIXct(expo2020$Date)
 min <- as.POSIXct("2018-04-26 03:00:00")
 max <- as.POSIXct("2018-05-27 03:00:00")

 # min <- as.Date("2018-04-24")
 # max <- as.Date("2018-05-28")

#### colours follow alphabetic order of the name of the pollutants

q <- ggplot(data = expo2020, 
            aes(Date, conc, fill = pollutant)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  geom_line(aes(Date, PM2.5), col="red", size = 2, linetype="twodash") +
  scale_fill_manual(values=c("#66b266", "#ff4c4c", "#b7b7b7", "#ff7f7f", "#eadbcc", "#0000ff",
                              "#008000", "#ff0000", "#00ffff", "#ffa500", "#5f345f", "#a52a2a",
                              "#6666ff", "#008000", "#ffffb2", "#808080", "#ffff00",
                               "#000000", "#ee82ee", "#ffd700")) +
  theme(legend.text = element_text(colour="black", size = 12, face = "bold")) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
  theme(axis.text.x=element_text(size=18,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                     
#  ylab("Ion Concentration (ug/m3)") +           
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, colour="black", vjust=0.5, size=20)) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust=1, size=13)) +
#  geom_text(aes(label = paste(round(conc), "%", sep = ""), y = pos), size = 9) +
  ggtitle("Chemical comp. of fine Particulate Matter (EXPO2020 Dubai)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  ylim(0, 500) +
# xlim(min, max)
scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%Y/%m/%d")) 
q


png(paste0(output_folder,"Chemicals_EXPO2020_May_2018.jpg"),
    width = 1500, height = 900, units = "px", pointsize = 13,
    bg = "white", res = 150)
print(q)
dev.off()

###############################################################################
###############################################################################
###############################################################################
###############################################################################
