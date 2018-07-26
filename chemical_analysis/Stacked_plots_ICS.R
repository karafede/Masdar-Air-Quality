

# Set-up ---------------------------
# Load packages
library(threadr)
library(dygraphs)
library(tidyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/ICS_chem_PM")

####################################################################################
# plot Mass concentrations MASDARD ROOF ############################################
####################################################################################

MASDAR_roof <- read.csv("MASDAR_roof_PMF.csv")
n_pollutants <- ncol(MASDAR_roof)-1

#gather data
MASDAR_roof <- gather(MASDAR_roof, "pollutant", "Date")
colnames(MASDAR_roof) <- c("Date", "pollutant", "conc")

str(MASDAR_roof)

MASDAR_roof <- MASDAR_roof %>%
  mutate(Date = mdy_hm(Date))

##################################################################
##################################################################

mass <- read.csv("Mass_conc.csv")
str(mass)
mass <- mass[, 1:3]

mass$DateTime <- mdy_hm(mass$DateTime)

# repeat mass values as many times the number of pollutants (n_pollutants)
mass_roof <- as.data.frame(rep(mass$masdar_roof, n_pollutants))

# bind mass to chemical analysis
MASDAR_roof <- cbind(MASDAR_roof, mass_roof)
colnames(MASDAR_roof) <- c("Date", "pollutant", "conc", "TSP_MASS")

STATS_ROOF <- mass %>%
  summarise(MEAN_roof = mean(masdar_roof, na.rm = TRUE),
            MEAN_field = mean(field, na.rm = TRUE))


# plot

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/ICS_chem_PM/IC_PM_samples/"

min <- as.POSIXct("2017-09-03 09:00:00") 
max <- as.POSIXct("2018-02-10 22:00:00") 

plot <- ggplot(mass, aes(DateTime, value)) + 
  theme_bw() +
  geom_line(aes(y = masdar_roof, col = "masdar_roof"), col="red", size = 3) +
  geom_line(aes(y = field, col = "field"), col="blue", size = 3) +
 # facet_wrap( ~ station) +
#  theme(legend.position="none") + 
 # theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste(TSP, " (µg/",m^3, ")", " monitored (weekly)"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=23, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=22, colour = "black")) +
  scale_colour_manual(values=c("red","green","blue")) +
  ylim(0, 250) +
  xlim(min, max) +
  geom_hline(yintercept = 150, size = 1.5) 
plot



png(paste0(output_folder,"time_series_TSP_concentration_Masdar.jpg"),
    width = 1200, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



######################################################################
# load MASDAR ROOF data ##############################################
######################################################################

MASDAR_roof <- MASDAR_roof %>% 
  mutate(Date = date(Date))

 str(MASDAR_roof)

# calculate midpoints of bars
MASDAR_roof <- ddply(MASDAR_roof, .(pollutant), 
                       transform, pos = cumsum(conc) - (0.5 * conc)
)

# make negative values == 0
MASDAR_roof[MASDAR_roof < 0] <- NA



# stacked plot

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/ICS_chem_PM/IC_PM_samples/"

min <- as.POSIXct("2017-09-03 09:00:00") 
max <- as.POSIXct("2018-02-10 22:00:00") 

min <- as.Date("2017-09-03")
max <- as.Date("2018-02-10")

#### colours follow alphabetic order of the name of the pollutants

q <- ggplot(data = MASDAR_roof, 
            aes(Date, conc, fill = pollutant)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  geom_line(aes(Date, TSP_MASS), col="red", size = 2, linetype="twodash") +
  scale_fill_manual(values=c("#66b266", "#ff4c4c", "#b7b7b7", "#ff7f7f", "#eadbcc", "#0000ff",
                              "#008000", "#ff0000", "#00ffff", "#ffa500", "#5f345f", "#a52a2a",
                              "#6666ff", "#008000", "#ffffb2", "#808080", "#ffff00",
                               "#000000", "#ee82ee", "#ffd700")) +
  theme(legend.text = element_text(colour="black", size = 12, face = "bold")) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
  theme(axis.text.x=element_text(size=18,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                     
#  ylab("Ion Concentration (ug/m3)") +           
  ylab(expression(paste("concentration", " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=22)) +
  theme(axis.text.x  = element_text(angle=90, vjust=0, size=25)) +
#  geom_text(aes(label = paste(round(conc), "%", sep = ""), y = pos), size = 9) +
  ggtitle("Chemical composition in Particulate Matter (Masdar Roof)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  ylim(0, 150) +
 xlim(min, max)
q


png(paste0(output_folder,"Chemicals_Masdar_roof_Sept_Oct_2017.jpg"),
    width = 1500, height = 900, units = "px", pointsize = 13,
    bg = "white", res = 150)
print(q)
dev.off()

###############################################################################
###############################################################################
###############################################################################
###############################################################################

##############################################################################
# load MASDAR FIELD STATION data #############################################
##############################################################################


####################################################################################
# plot Mass concentrations MASDARD FIELD ###########################################
####################################################################################

MASDAR_field <- read.csv("MASDAR_field_PMF.csv")
n_pollutants <- ncol(MASDAR_field)-1

#gather data
MASDAR_field <- gather(MASDAR_field, "pollutant", "Date")
colnames(MASDAR_field) <- c("Date", "pollutant", "conc")

str(MASDAR_field)

MASDAR_field <- MASDAR_field %>%
  mutate(Date = mdy_hm(Date))

##################################################################
##################################################################

mass <- read.csv("Mass_conc.csv")
str(mass)
mass <- mass[, 1:3]

mass$DateTime <- mdy_hm(mass$DateTime)

# repeat mass values as many times the number of pollutants (n_pollutants)
mass_field <- as.data.frame(rep(mass$field, n_pollutants))

# bind mass to chemical analysis
MASDAR_field <- cbind(MASDAR_field, mass_field)
colnames(MASDAR_field) <- c("Date", "pollutant", "conc", "TSP_MASS")

STATS_FIELD <- mass %>%
  summarise(MEAN_roof = mean(masdar_roof, na.rm = TRUE),
            MEAN_field = mean(field, na.rm = TRUE))


##################################################################
##################################################################

MASDAR_field <- MASDAR_field %>% 
  mutate(Date = date(Date))


# calculate midpoints of bars
MASDAR_field <- ddply(MASDAR_field, .(pollutant), 
                     transform, pos = cumsum(conc) - (0.5 * conc)
)

# stacked plot

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/ICS_chem_PM/IC_PM_samples/"

min <- as.POSIXct("2017-09-03 09:00:00") 
max <- as.POSIXct("2018-02-10 22:00:00") 

min <- as.Date("2017-09-03")
max <- as.Date("2018-02-10")



q <- ggplot(data = MASDAR_field, 
            aes(Date, conc, fill = pollutant)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  geom_line(aes(Date, TSP_MASS), col="blue", size = 2, linetype="twodash") +
  scale_fill_manual(values=c("#66b266", "#ff4c4c", "#b7b7b7", "#ff7f7f", "#eadbcc", "#0000ff",
                             "#008000", "#ff0000", "#00ffff", "#ffa500", "#5f345f", "#a52a2a",
                             "#6666ff", "#008000", "#ffffb2", "#808080", "#ffff00",
                             "#000000", "#ee82ee", "#ffd700")) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5)) +
  theme(axis.text.x=element_text(size=18,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                     
  #  ylab("Ion Concentration (ug/m3)") +           
  ylab(expression(paste("concentration", " (µg/",m^3, ")"),size=18)) + 
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=22),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=22)) +
  theme(axis.text.x  = element_text(angle=90, vjust=0, size=25)) +
  #  geom_text(aes(label = paste(round(conc), "%", sep = ""), y = pos), size = 9) +
  ggtitle("Chemical composition in Particulate Matter (Masdar field)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20))+
  ylim(0, 220) +
  xlim(min, max)
q


png(paste0(output_folder,"Chemicals_Masdar_field_Sept_Oct_2017.jpg.jpg"),
    width = 1500, height = 900, units = "px", pointsize = 13,
    bg = "white", res = 150)
print(q)
dev.off()

