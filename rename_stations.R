

library(readr)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates")

EAD_2013 <- read_csv("database_EAD_2013_hourly.csv")
EAD_2014 <- read_csv("database_EAD_2014_hourly.csv")
EAD_2015 <- read_csv("database_EAD_2015_hourly.csv")
EAD_2016 <- read_csv("database_EAD_2016_hourly.csv")


EAD_2013$Site  <- ifelse(grepl("ALAinIslamicIns", EAD_2013$Site, ignore.case = TRUE), 
                         "Al Ain Islamic Ins", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("ALAinIslamicIns", EAD_2014$Site, ignore.case = TRUE), 
                         "Al Ain Islamic Ins", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("ALAinIslamicIns", EAD_2015$Site, ignore.case = TRUE), 
                                 "Al Ain Islamic Ins", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("ALAinIslamicIns", EAD_2016$Site, ignore.case = TRUE), 
                         "Al Ain Islamic Ins", EAD_2016$Site)



EAD_2013$Site  <- ifelse(grepl("ALAinStreet", EAD_2013$Site, ignore.case = TRUE), 
                         "Al Ain Street", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("ALAinStreet", EAD_2014$Site, ignore.case = TRUE), 
                         "Al Ain Street", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("ALAinStreet", EAD_2015$Site, ignore.case = TRUE), 
                         "Al Ain Street", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("ALAinStreet", EAD_2016$Site, ignore.case = TRUE), 
                         "Al Ain Street", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("AlMafraq", EAD_2013$Site, ignore.case = TRUE), 
                         "Al Mafraq", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("AlMafraq", EAD_2014$Site, ignore.case = TRUE), 
                         "Al Mafraq", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("AlMafraq", EAD_2015$Site, ignore.case = TRUE), 
                         "Al Mafraq", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("AlMafraq", EAD_2016$Site, ignore.case = TRUE), 
                         "Al Mafraq", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("AlQua0x27a", EAD_2013$Site, ignore.case = TRUE), 
                         "Al Qua'a", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("AlQua0x27a", EAD_2014$Site, ignore.case = TRUE), 
                         "Al Qua'a", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("AlQua0x27a", EAD_2015$Site, ignore.case = TRUE), 
                         "Al Qua'a", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("AlQua0x27a", EAD_2016$Site, ignore.case = TRUE), 
                         "Al Qua'a", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("AlRuwais", EAD_2013$Site, ignore.case = TRUE), 
                         "Al Ruwais", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("AlRuwais", EAD_2014$Site, ignore.case = TRUE), 
                         "Al Ruwais", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("AlRuwais", EAD_2015$Site, ignore.case = TRUE), 
                         "Al Ruwais", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("AlRuwais", EAD_2016$Site, ignore.case = TRUE), 
                         "Al Ruwais", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("AlTawia", EAD_2013$Site, ignore.case = TRUE), 
                         "Al Tawia", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("AlTawia", EAD_2014$Site, ignore.case = TRUE), 
                         "Al Tawia", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("AlTawia", EAD_2015$Site, ignore.case = TRUE), 
                         "Al Tawia", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("AlTawia", EAD_2016$Site, ignore.case = TRUE), 
                         "Al Tawia", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("BainAljesrain", EAD_2013$Site, ignore.case = TRUE), 
                         "Bain Al Jesrain", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("BainAljesrain", EAD_2014$Site, ignore.case = TRUE), 
                         "Bain Al Jesrain", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("BainAljesrain", EAD_2015$Site, ignore.case = TRUE), 
                         "Bain Al Jesrain", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("BainAljesrain", EAD_2016$Site, ignore.case = TRUE), 
                         "Bain Al Jesrain", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("baniyasSchool", EAD_2013$Site, ignore.case = TRUE), 
                         "Baniyas School", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("baniyasSchool", EAD_2014$Site, ignore.case = TRUE), 
                         "Baniyas School", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("baniyasSchool", EAD_2015$Site, ignore.case = TRUE), 
                         "Baniyas School", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("baniyasSchool", EAD_2016$Site, ignore.case = TRUE), 
                         "Baniyas School", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("BidaZayed", EAD_2013$Site, ignore.case = TRUE), 
                         "Bida Zayed", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("BidaZayed", EAD_2014$Site, ignore.case = TRUE), 
                         "Bida Zayed", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("BidaZayed", EAD_2015$Site, ignore.case = TRUE), 
                         "Bida Zayed", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("BidaZayed", EAD_2016$Site, ignore.case = TRUE), 
                         "Bida Zayed", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("E11Road", EAD_2013$Site, ignore.case = TRUE), 
                         "E11 Road", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("E11Road", EAD_2014$Site, ignore.case = TRUE), 
                         "E11 Road", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("E11Road", EAD_2015$Site, ignore.case = TRUE), 
                         "E11 Road", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("E11Road", EAD_2016$Site, ignore.case = TRUE), 
                         "E11 Road", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("GayathiSchool", EAD_2013$Site, ignore.case = TRUE), 
                         "Gayathi School", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("GayathiSchool", EAD_2014$Site, ignore.case = TRUE), 
                         "Gayathi School", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("GayathiSchool", EAD_2015$Site, ignore.case = TRUE), 
                         "Gayathi School", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("GayathiSchool", EAD_2016$Site, ignore.case = TRUE), 
                         "Gayathi School", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("Habshan", EAD_2013$Site, ignore.case = TRUE), 
                         "Habshan", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("Habshan", EAD_2014$Site, ignore.case = TRUE), 
                         "Habshan", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("Habshan", EAD_2015$Site, ignore.case = TRUE), 
                         "Habshan", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("Habshan", EAD_2016$Site, ignore.case = TRUE), 
                         "Habshan", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("HamdanStreet", EAD_2013$Site, ignore.case = TRUE), 
                         "Hamdan Street", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("HamdanStreet", EAD_2014$Site, ignore.case = TRUE), 
                         "Hamdan Street", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("HamdanStreet", EAD_2015$Site, ignore.case = TRUE), 
                         "Hamdan Street", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("HamdanStreet", EAD_2016$Site, ignore.case = TRUE), 
                         "Hamdan Street", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("KhadejaPrimarySchool", EAD_2013$Site, ignore.case = TRUE), 
                         "Khadeja Primary School", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("KhadejaPrimarySchool", EAD_2014$Site, ignore.case = TRUE), 
                         "Khadeja Primary School", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("KhadejaPrimarySchool", EAD_2015$Site, ignore.case = TRUE), 
                         "Khadeja Primary School", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("KhadejaPrimarySchool", EAD_2016$Site, ignore.case = TRUE), 
                         "Khadeja Primary School", EAD_2016$Site)



EAD_2013$Site  <- ifelse(grepl("KhalifaCityA", EAD_2013$Site, ignore.case = TRUE), 
                         "Khalifa City A", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("KhalifaCityA", EAD_2014$Site, ignore.case = TRUE), 
                         "Khalifa City A", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("KhalifaCityA", EAD_2015$Site, ignore.case = TRUE), 
                         "Khalifa City A", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("KhalifaCityA", EAD_2016$Site, ignore.case = TRUE), 
                         "Khalifa City A", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("KhalifaHighSchool", EAD_2013$Site, ignore.case = TRUE), 
                         "Khalifa High School", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("KhalifaHighSchool", EAD_2014$Site, ignore.case = TRUE), 
                         "Khalifa High School", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("KhalifaHighSchool", EAD_2015$Site, ignore.case = TRUE), 
                         "Khalifa High School", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("KhalifaHighSchool", EAD_2016$Site, ignore.case = TRUE), 
                         "Khalifa High School", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("LiwaOasis", EAD_2013$Site, ignore.case = TRUE), 
                         "Liwa Oasis", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("LiwaOasis", EAD_2014$Site, ignore.case = TRUE), 
                         "Liwa Oasis", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("LiwaOasis", EAD_2015$Site, ignore.case = TRUE), 
                         "Liwa Oasis", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("LiwaOasis", EAD_2016$Site, ignore.case = TRUE), 
                         "Liwa Oasis", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("Mussafah", EAD_2013$Site, ignore.case = TRUE), 
                         "Mussafah", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("Mussafah", EAD_2014$Site, ignore.case = TRUE), 
                         "Mussafah", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("Mussafah", EAD_2015$Site, ignore.case = TRUE), 
                         "Mussafah", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("Mussafah", EAD_2016$Site, ignore.case = TRUE), 
                         "Mussafah", EAD_2016$Site)


EAD_2013$Site  <- ifelse(grepl("Sweihan", EAD_2013$Site, ignore.case = TRUE), 
                         "Sweihan", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("Sweihan", EAD_2014$Site, ignore.case = TRUE), 
                         "Sweihan", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("Sweihan", EAD_2015$Site, ignore.case = TRUE), 
                         "Sweihan", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("Sweihan", EAD_2016$Site, ignore.case = TRUE), 
                         "Sweihan", EAD_2016$Site)

EAD_2013$Site  <- ifelse(grepl("Zakher", EAD_2013$Site, ignore.case = TRUE), 
                         "Zakher", EAD_2013$Site)
EAD_2014$Site  <- ifelse(grepl("Zakher", EAD_2014$Site, ignore.case = TRUE), 
                         "Zakher", EAD_2014$Site)
EAD_2015$Site  <- ifelse(grepl("Zakher", EAD_2015$Site, ignore.case = TRUE), 
                         "Zakher", EAD_2015$Site)
EAD_2016$Site  <- ifelse(grepl("Zakher", EAD_2016$Site, ignore.case = TRUE), 
                         "Zakher", EAD_2016$Site)

write_csv(EAD_2013, "database_EAD_2013_hourly_renamed.csv" )
write_csv(EAD_2014, "database_EAD_2014_hourly_renamed.csv" )
write_csv(EAD_2015, "database_EAD_2015_hourly_renamed.csv" )
write_csv(EAD_2016, "database_EAD_2016_hourly_renamed.csv" )


########################
# for the NCMS station #
########################


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates")

NCMS_2013 <- read_csv("database_NCMS_2013_hourly.csv")
NCMS_2014 <- read_csv("database_NCMS_2014_hourly.csv")
NCMS_2015 <- read_csv("database_NCMS_2015_hourly.csv")
NCMS_2016 <- read_csv("database_NCMS_2016_hourly_.csv")


NCMS_2013$Site  <- ifelse(grepl("AlHamriyah", NCMS_2013$Site, ignore.case = TRUE), 
                          "Al Hamriyah", NCMS_2013$Site)
NCMS_2014$Site  <- ifelse(grepl("AlHamriyah", NCMS_2014$Site, ignore.case = TRUE), 
                          "Al Hamriyah", NCMS_2014$Site)
NCMS_2015$Site  <- ifelse(grepl("AlHamriyah", NCMS_2015$Site, ignore.case = TRUE), 
                          "Al Hamriyah", NCMS_2015$Site)
NCMS_2016$Site  <- ifelse(grepl("AlHamriyah", NCMS_2016$Site, ignore.case = TRUE), 
                          "Al Hamriyah", NCMS_2016$Site)



NCMS_2013$Site  <- ifelse(grepl("Kalba", NCMS_2013$Site, ignore.case = TRUE), 
                          "Kalba", NCMS_2013$Site)
NCMS_2014$Site  <- ifelse(grepl("Kalba", NCMS_2014$Site, ignore.case = TRUE), 
                          "Kalba", NCMS_2014$Site)
NCMS_2015$Site  <- ifelse(grepl("Kalba", NCMS_2015$Site, ignore.case = TRUE), 
                          "Kalba", NCMS_2015$Site)
NCMS_2016$Site  <- ifelse(grepl("Kalba", NCMS_2016$Site, ignore.case = TRUE), 
                          "Kalba", NCMS_2016$Site)


NCMS_2013$Site  <- ifelse(grepl("Rashidyah", NCMS_2013$Site, ignore.case = TRUE), 
                          "Rashidyah", NCMS_2013$Site)
NCMS_2014$Site  <- ifelse(grepl("Rashidyah", NCMS_2014$Site, ignore.case = TRUE), 
                          "Rashidyah", NCMS_2014$Site)
NCMS_2015$Site  <- ifelse(grepl("Rashidyah", NCMS_2015$Site, ignore.case = TRUE), 
                          "Rashidyah", NCMS_2015$Site)
NCMS_2016$Site  <- ifelse(grepl("Rashidyah", NCMS_2016$Site, ignore.case = TRUE), 
                          "Rashidyah", NCMS_2016$Site)


NCMS_2013$Site  <- ifelse(grepl("ELdErLyHouse", NCMS_2013$Site, ignore.case = TRUE), 
                          "Elderly House", NCMS_2013$Site)
NCMS_2014$Site  <- ifelse(grepl("ELdErLyHouse", NCMS_2014$Site, ignore.case = TRUE), 
                          "Elderly House", NCMS_2014$Site)
NCMS_2015$Site  <- ifelse(grepl("ELdErLyHouse", NCMS_2015$Site, ignore.case = TRUE), 
                          "Elderly House", NCMS_2015$Site)
NCMS_2016$Site  <- ifelse(grepl("ELdErLyHouse", NCMS_2016$Site, ignore.case = TRUE), 
                          "Elderly House", NCMS_2016$Site)


NCMS_2013$Site  <- ifelse(grepl("AlJeer", NCMS_2013$Site, ignore.case = TRUE), 
                          "Al Jeer", NCMS_2013$Site)
NCMS_2014$Site  <- ifelse(grepl("AlJeer", NCMS_2014$Site, ignore.case = TRUE), 
                          "Al Jeer", NCMS_2014$Site)
NCMS_2015$Site  <- ifelse(grepl("AlJeer", NCMS_2015$Site, ignore.case = TRUE), 
                          "Al Jeer", NCMS_2015$Site)
NCMS_2016$Site  <- ifelse(grepl("AlJeer", NCMS_2016$Site, ignore.case = TRUE), 
                          "Al Jeer", NCMS_2016$Site)


NCMS_2013$Site  <- ifelse(grepl("Burairat", NCMS_2013$Site, ignore.case = TRUE), 
                          "Burairat", NCMS_2013$Site)
NCMS_2014$Site  <- ifelse(grepl("Burairat", NCMS_2014$Site, ignore.case = TRUE), 
                          "Burairat", NCMS_2014$Site)
NCMS_2015$Site  <- ifelse(grepl("Burairat", NCMS_2015$Site, ignore.case = TRUE), 
                          "Burairat", NCMS_2015$Site)
NCMS_2016$Site  <- ifelse(grepl("Burairat", NCMS_2016$Site, ignore.case = TRUE), 
                          "Burairat", NCMS_2016$Site)


NCMS_2013$Site  <- ifelse(grepl("AlQasimiyah", NCMS_2013$Site, ignore.case = TRUE), 
                          "Al Qasimiyah", NCMS_2013$Site)
NCMS_2014$Site  <- ifelse(grepl("AlQasimiyah", NCMS_2014$Site, ignore.case = TRUE), 
                          "Al Qasimiyah", NCMS_2014$Site)
NCMS_2015$Site  <- ifelse(grepl("AlQasimiyah", NCMS_2015$Site, ignore.case = TRUE), 
                          "Al Qasimiyah", NCMS_2015$Site)
NCMS_2016$Site  <- ifelse(grepl("Al Qasimiyah", NCMS_2016$Site, ignore.case = TRUE), 
                          "Al Qasimiyah", NCMS_2016$Site)


NCMS_2013$Site  <- ifelse(grepl("Ghalilah", NCMS_2013$Site, ignore.case = TRUE), 
                          "Ghalilah", NCMS_2013$Site)
NCMS_2014$Site  <- ifelse(grepl("Ghalilah", NCMS_2014$Site, ignore.case = TRUE), 
                          "Ghalilah", NCMS_2014$Site)
NCMS_2015$Site  <- ifelse(grepl("Ghalilah", NCMS_2015$Site, ignore.case = TRUE), 
                          "Ghalilah", NCMS_2015$Site)
NCMS_2016$Site  <- ifelse(grepl("Ghalilah", NCMS_2016$Site, ignore.case = TRUE), 
                          "Ghalilah", NCMS_2016$Site)


write_csv(NCMS_2013, "database_NCMS_2013_hourly_renamed.csv" )
write_csv(NCMS_2014, "database_NCMS_2014_hourly_renamed.csv" )
write_csv(NCMS_2015, "database_NCMS_2015_hourly_renamed.csv" )
write_csv(NCMS_2016, "database_NCMS_2016_hourly_renamed.csv" )


########################
# for the DM station #
########################

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates")

DM_2013 <- read_csv("database_DM_2013_hourly.csv")
DM_2014 <- read_csv("database_DM_2014_hourly.csv")
DM_2015 <- read_csv("database_DM_2015_hourly.csv")
DM_2016 <- read_csv("database_DM_2016_hourly.csv")


DM_2013$Site  <- ifelse(grepl("Deira", DM_2013$Site, ignore.case = TRUE), 
                        "Deira", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("Deira", DM_2014$Site, ignore.case = TRUE), 
                        "Deira", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("Deira", DM_2015$Site, ignore.case = TRUE), 
                        "Deira", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("Deira", DM_2016$Site, ignore.case = TRUE), 
                        "Deira", DM_2016$Site)



DM_2013$Site  <- ifelse(grepl("Karama", DM_2013$Site, ignore.case = TRUE), 
                        "Karama", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("Karama", DM_2014$Site, ignore.case = TRUE), 
                        "Karama", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("Karama", DM_2015$Site, ignore.case = TRUE), 
                        "Karama", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("Karama", DM_2016$Site, ignore.case = TRUE), 
                        "Karama", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("Zabeel", DM_2013$Site, ignore.case = TRUE), 
                        "Zabeel", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("Zabeel", DM_2014$Site, ignore.case = TRUE), 
                        "Zabeel", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("Zabeel", DM_2015$Site, ignore.case = TRUE), 
                        "Zabeel", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("Zabeel", DM_2016$Site, ignore.case = TRUE), 
                        "Zabeel", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("JEBELALIVILLAGE", DM_2013$Site, ignore.case = TRUE), 
                        "JEBEL ALI VILLAGE", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("JEBELALIVILLAGE", DM_2014$Site, ignore.case = TRUE), 
                        "JEBEL ALI VILLAGE", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("JEBELALIVILLAGE", DM_2015$Site, ignore.case = TRUE), 
                        "JEBEL ALI VILLAGE", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("JEBELALIVILLAGE", DM_2016$Site, ignore.case = TRUE), 
                        "JEBEL ALI VILLAGE", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("EMIRATESHILLS", DM_2013$Site, ignore.case = TRUE), 
                        "EMIRATES HILLS", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("EMIRATESHILLS", DM_2014$Site, ignore.case = TRUE), 
                        "EMIRATES HILLS", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("EMIRATESHILLS", DM_2015$Site, ignore.case = TRUE), 
                        "EMIRATES HILLS", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("EMIRATESHILLS", DM_2016$Site, ignore.case = TRUE), 
                        "EMIRATES HILLS", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("safa", DM_2013$Site, ignore.case = TRUE), 
                        "Safa", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("safa", DM_2014$Site, ignore.case = TRUE), 
                        "Safa", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("safa", DM_2015$Site, ignore.case = TRUE), 
                        "Safa", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("safa", DM_2016$Site, ignore.case = TRUE), 
                        "Safa", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("DUBAIAIRPORT", DM_2013$Site, ignore.case = TRUE), 
                        "DUBAI AIR PORT", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("DUBAIAIRPORT", DM_2014$Site, ignore.case = TRUE), 
                        "DUBAI AIR PORT", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("DUBAIAIRPORT", DM_2015$Site, ignore.case = TRUE), 
                        "DUBAI AIR PORT", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("Al Qasimiyah", DM_2016$Site, ignore.case = TRUE), 
                        "DUBAI AIR PORT", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("MUSHRIF", DM_2013$Site, ignore.case = TRUE), 
                        "MUSHRIF", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("MUSHRIF", DM_2014$Site, ignore.case = TRUE), 
                        "MUSHRIF", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("MUSHRIF", DM_2015$Site, ignore.case = TRUE), 
                        "MUSHRIF", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("MUSHRIF", DM_2016$Site, ignore.case = TRUE), 
                        "MUSHRIF", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("Hatta", DM_2013$Site, ignore.case = TRUE), 
                        "Hatta", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("Hatta", DM_2014$Site, ignore.case = TRUE), 
                        "Hatta", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("Hatta", DM_2015$Site, ignore.case = TRUE), 
                        "Hatta", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("Hatta", DM_2016$Site, ignore.case = TRUE), 
                        "Hatta", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("Warsan", DM_2013$Site, ignore.case = TRUE), 
                        "Warsan", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("Warsan", DM_2014$Site, ignore.case = TRUE), 
                        "Warsan", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("Warsan", DM_2015$Site, ignore.case = TRUE), 
                        "Warsan", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("Warsan", DM_2016$Site, ignore.case = TRUE), 
                        "Warsan", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("JEBELALIPORT", DM_2013$Site, ignore.case = TRUE), 
                        "JEBEL ALI PORT", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("JEBELALIPORT", DM_2014$Site, ignore.case = TRUE), 
                        "JEBEL ALI PORT", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("JEBELALIPORT", DM_2015$Site, ignore.case = TRUE), 
                        "JEBEL ALI PORT", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("JEBELALIPORT", DM_2016$Site, ignore.case = TRUE), 
                        "JEBEL ALI PORT", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", DM_2013$Site, ignore.case = TRUE), 
                        "SHK. ZAYED ROAD", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", DM_2014$Site, ignore.case = TRUE), 
                        "SHK. ZAYED ROAD", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", DM_2015$Site, ignore.case = TRUE), 
                        "SHK. ZAYED ROAD", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", DM_2016$Site, ignore.case = TRUE), 
                        "SHK. ZAYED ROAD", DM_2016$Site)


DM_2013$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", DM_2013$Site, ignore.case = TRUE), 
                        "SHK. MOHD. BIN ZAYED ROAD", DM_2013$Site)
DM_2014$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", DM_2014$Site, ignore.case = TRUE), 
                        "SHK. MOHD. BIN ZAYED ROAD", DM_2014$Site)
DM_2015$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", DM_2015$Site, ignore.case = TRUE), 
                        "SHK. MOHD. BIN ZAYED ROAD", DM_2015$Site)
DM_2016$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", DM_2016$Site, ignore.case = TRUE), 
                        "SHK. MOHD. BIN ZAYED ROAD", DM_2016$Site)


write_csv(DM_2013, "database_DM_2013_hourly_renamed.csv" )
write_csv(DM_2014, "database_DM_2014_hourly_renamed.csv" )
write_csv(DM_2015, "database_DM_2015_hourly_renamed.csv" )
write_csv(DM_2016, "database_DM_2016_hourly_renamed.csv" )


