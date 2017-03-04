walash_outlier <- function(to_walash ){
  
  # separating the yearly data to quarterly
  to_walash <- to_walash %>%
    mutate(date = mdy(DateTime, tz = "UTC"),
           year = year(date),
           month = month(date)) %>%
    # dplyr:: select(date,
    #                Site,
    #                year,
    #                month,
    #                Pollutant,
    #                Value,
    #                Cap) %>%
    # filter(Pollutant == "PM2.5")
  
  to_walash$to_walash <- character(length = nrow(to_walash))
  to_walash$quarter[to_walash$month %in% month.abb[c(1:3)]] <- "Q1"
  to_walash$quarter[to_walash$month %in% month.abb[c(4:6)]] <- "Q2"
  to_walash$quarter[to_walash$month %in% month.abb[c(7:9)]] <- "Q3"
  to_walash$quarter[to_walash$month %in% month.abb[c(10:12)]] <- "Q4"
  
  
  zzz<-to_walash [c("DateTime", "Value", "quarter")]
  data_out_NA <- na.omit(zzz)
  Q_identifier <- unique(raw_sel$quarter)
  for (kkk in Q_identifier){
    zzz_Q<- zzz %>%
      filter(quarter == kkk)
    
    if (nrow(data_out_NA) == 0){
      output_1<- to_walash
      output_2<- 0
    } else {
      n=nrow(data_out_NA)
      # getting the significance level 0.05, 0.1 or not applicaple
      if (n > 220){
        Alpha = 0.05
      }
      if (n <= 220 & n > 60){
        Alpha = 0.1
      } 
      if ( n < 60){
        output_2<- 0
      }
      if (exists("output_2")){
      } else{
        # get the possible outliers from the timeseries
        box_objec<-boxplot(data_out_NA$Value)
        r_box<-length(box_objec$out)
        if (r_box>=1) {
          sort_ind <- order(data_out_NA$Value)
          sorted_data<-data_out_NA[sort_ind,]
          outliers_dat<- data.frame()
          x_ss<- data.frame()
          x_ll<- data.frame()
          for (r in 1:r_box){
            c = ceiling(sqrt(2*n))
            k = r + c
            b_Squ = 1/Alpha
            a = (1+b_Squ*sqrt((c-b_Squ)/(c-1)))/(c-b_Squ-1)
            X_Small = sorted_data$Value[r] - (1+a)*sorted_data$Value[r+1] + a*sorted_data$Value[k]
            if (X_Small < 0){
              outliers_dat<-rbind(outliers_dat,sorted_data[r,])
              x_ss<- rbind(x_ss,X_Small)
            }
            x_Large = sorted_data$Value[n+1-r] - (1+a)*sorted_data$Value[n-r] + a*sorted_data$Value[n+1-k]
            if (x_Large > 0){
              outliers_dat<-rbind(outliers_dat,sorted_data[r,])
              x_ll<- rbind(x_ll,x_Large)
            }
          }
          
          
          for (dave in 1:nrow(outliers_dat)){ 
            find_out<- outliers_dat$DateTime[dave]==zzz$DateTime
            ind_to_rep<-which(find_out)
            zzz$Value[ind_to_rep]= NA
          }
          output_4<-r_box
          output_3<-nrow(outliers_dat)
          output_1<-zzz
          output_5<-nrow(x_ss)
          output_6<-nrow(x_ll)
        } else {
          output_2<-2
        }
        
      }
    }
  } 
  if (exists("output_4")){
  } else {
    output_4<- "no Suspected"
  }
  if (exists("output_5")){
  } else {
    output_5<- "no Suspected"
  }
  if (exists("output_6")){
  } else {
    output_6<- "no Suspected"
  }
  
  if (exists("output_3")){
  } else {
    output_3<- NaN
  }
  if (exists("output_2")){
  } else {
    output_2<- 1
  }
  if(exists("output_1")){
  } else {
    output_1<-to_walash
  }
  output<- list(output_1,output_2,output_3,output_4,output_5,output_6)
  return(output)

}

