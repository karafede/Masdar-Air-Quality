


library(readr)
library(dplyr)
library(threadr)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates")

EAD_2016 <- read_csv("database_EAD_2016_hourly.csv")


LiwaOasis <- EAD_2016 %>%
  filter(EAD_2016$Site == "LiwaOasis")



AlTawia <- EAD_2016 %>%
  filter(EAD_2016$Site == "AlTawia")


Zakher <- EAD_2016 %>%
  filter(EAD_2016$Site == "Zakher")


E11road <- EAD_2016 %>%
  filter(EAD_2016$Site == "E11Road")
