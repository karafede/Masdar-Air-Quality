library(dplyr)
setwd("C:/Users/dtghebreyesus/Documents/R files")
source("walash_fuunction.r")

dawit <- read.csv('D:/database_DM_2013_hourly.csv')
raw_sel <- subset(dawit, select=c("DateTime", "Site","Pollutant","Value"))
station <- unique(raw_sel$Site)
david<- vector()
for (i in station){
  i<-"Deira" # to be deleted
  stat<- filter(raw_sel, Site == i )
  pollu <- unique(stat$Pollutant)
  for(j in pollu){
    j<-"NO2"# to be deleted
    to_walash <- filter(stat, Pollutant == j )
    if (is.null(to_walash)){
    } else {
      output_OL <- walash_outlier(to_walash) # walash function which have two outcomes as a list
      output_error<- as.numeric(output_OL[2])
      if (output_error==1){
        output_OL<- as.data.frame(output_OL[1])
        dave <- (dawit$Site==i & dawit$Pollutant == j )
        ind_true <- which(dave)
        # q=1 k<-ind_true[1] l=2
        dawit$Value[ind_true]<- output_OL$Value
        
        
        
        for (k in ind_true){
          for (l in 1:length(ind_true)){
            if (dawit$DateTime[k] == output_OL$DateTime[l]){
              dawit$Value[k] <- output_OL$Value[l]
            }
          }
        }
      }else {
        # dave <- (dawit$Site==i & dawit$Pollutant == j )
        # ind_true <- which(dave)
        # # q=1
        # for (k in ind_true){
        #   for (l in 1:length(ind_true)){
        #     if (dawit$DateTime[k] == output_OL$DateTime[l]){
        #       dawit$Value[k] <- output_OL$Value[l]
        #     }
        #   }
        # }
      }
    }
  }
}
write.csv(dawit, file = "database_DM_2013_hourly_filtered.csv")