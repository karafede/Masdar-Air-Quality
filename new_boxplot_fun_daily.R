walash_outlier <- function(to_walash, size_whskr ){
  
  # separating the yearly data to quarterly
  to_walash <- to_walash %>%
    mutate(year = year(Date),
           month = month(Date)) #%>%
  
  
  to_walash$quarter <- character(length = nrow(to_walash))
  to_walash$quarter[to_walash$month %in% c(1:3)] <- "Q1"
  to_walash$quarter[to_walash$month %in% c(4:6)] <- "Q2"
  to_walash$quarter[to_walash$month %in% c(7:9)] <- "Q3"
  to_walash$quarter[to_walash$month %in% c(10:12)] <- "Q4"
  
  # end of qauarterly
  
  
  # preparing and initialization
  
  zzz<-to_walash [c("Date", "Daily_mean", "quarter")]
  Q_identifier <- unique(zzz$quarter)
  data_out_NA <- na.omit(zzz)
  Out_1<-data.frame()
  Out_3<-data.frame()
  
  check_error <- 0
  check_error2<- 0
  check_error3<- 0
  
  # start of the loop in quarterly
  
  for (kkk in Q_identifier){
    # kkk<- "Q4"
    zzz_Q<- zzz %>%
      filter(quarter == kkk)
    Q_NA <- na.omit(zzz_Q)
    if (nrow(Q_NA) == 0){
      output_1<- zzz_Q # same output 
      #output_22<- 0
      output_3<- 0 # number of outliers
      output_2<- 0 # indicator if output_2==1 there are outliers output_2==2 no outliers output_2==0 all of the values are NaN
      
    } else{
    #checks if the station have values or all of them are NA
    if (nrow(data_out_NA) == 0){
      
      output_1<- zzz_Q # same output 
      #output_22<- 0
      output_3<- 0 # number of outliers
      output_2<- 0 # indicator if output_2==1 there are outliers output_2==2 no outliers output_2==0 all of the values are NaN
      
    } 
    else {                  # if the data have values 
      if (exists("output_2")){
      } else{
        # get the possible outliers from the timeseries
        box_objec<-boxplot(zzz_Q$Daily_mean, range = size_whskr)
        ind_no<- which(zzz_Q$Daily_mean %in% box_objec$out)
        pos_out<-data.frame()
        ind_no_df<-as.data.frame(ind_no)
        val<-box_objec$out
        pos_out<- cbind(ind_no_df,val)
        r_box<-nrow(pos_out)
        #outliers_dat<-rbind(outliers_dat,sorted_data[r,])
        zzz_Q$Daily_mean[ind_no]= NA
        output_3<-nrow(ind_no_df)
        output_1<-zzz_Q
        
        # indicator 
        if (nrow(ind_no_df) >= 1){
          output_2<- 1
        }else if (nrow(ind_no_df)==0){
          output_2<- 2
        }
        
      }
    }

    }
    
    if (exists("output_2")){     #indicator if output_2==1 there are outliers output_2==2 no outliers output_2==0 all of the values are NaN
      if (output_2==0){
        check_error<-1+check_error
      }
      if(output_2==1){
        check_error2 <- 1 + check_error2 
        
      }
      if(output_2==2){
        check_error3<-1+check_error3
      }
    }
    Out_3<-rbind(Out_3,output_3)
    Out_1<-rbind(Out_1,output_1)
    remove(output_3,output_1,output_2)
  }
## end of loop for the quarterly

if (sum(Out_3,na.rm = T) > 0){
  output_33<-sum(Out_3,na.rm = T)
} else {
  output_33<- 0
}

if (check_error==4){
  output_22<- 0
} 
if (check_error3==4 | sum(check_error,check_error3)==4){
  output_22<- 2
} 
if (check_error2 >=1){
  output_22<- 1
} 
#if(exists("Out_1")){
  output_11<-Out_1
# } else {
#   output_11<-to_walash
# }
output<- list(output_11,output_22,output_33 )
return(output)
}

  
  
  