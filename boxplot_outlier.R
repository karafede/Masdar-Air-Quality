walash_outlier <- function(to_walash, size_whskr ){
  
  # separating the yearly data to quarterly
  to_walash <- to_walash %>%
    mutate(year = year(DateTime),
           month = month(DateTime)) #%>%
  
  
  to_walash$quarter <- character(length = nrow(to_walash))
  to_walash$quarter[to_walash$month %in% c(1:3)] <- "Q1"
  to_walash$quarter[to_walash$month %in% c(4:6)] <- "Q2"
  to_walash$quarter[to_walash$month %in% c(7:9)] <- "Q3"
  to_walash$quarter[to_walash$month %in% c(10:12)] <- "Q4"
  
  
  zzz<-to_walash [c("DateTime", "Value", "quarter")]
  Q_identifier <- unique(zzz$quarter)
  data_out_NA <- na.omit(zzz)
  Out_1<-data.frame()
  Out_3<-data.frame()
  check_error <- 0
  check_error2<- 0
  check_error3<- 0
  
  for (kkk in Q_identifier){
    # kkk<- "Q1"
    zzz_Q<- zzz %>%
      filter(quarter == kkk)
    
    #checks if the station have values or all of them are NA
    if (nrow(data_out_NA) == 0){
      output_1<- zzz_Q
      output_22<- 0
      output_3<- 0
      output_2<- 0
      
    } 
    else {
      if (exists("output_2")){
      } else{
        # get the possible outliers from the timeseries
        box_objec<-boxplot(zzz_Q$Value, range = 1.5)#size_whskr)
        ind_no<- which(zzz_Q$Value %in% box_objec$out)
        pos_out<-data.frame()
        ind_no_df<-as.data.frame(ind_no)
        val<-box_objec$out
        pos_out<- cbind(ind_no_df,val)
        r_box<-nrow(pos_out)
        #outliers_dat<-rbind(outliers_dat,sorted_data[r,])
        zzz_Q$Value[ind_no]= NA
        output_3<-nrow(ind_no_df)
        output_1<-zzz_Q
        if (nrow(ind_no_df) >= 1){
          output_2<- 1
        }else if (nrow(ind_no_df)==0){
          output_2<- 2
          output_3<- 0
        }
        
        check_error<-1+check_error
        
        
      }
    
    if (exists("output_2")){
      if (output_2==0){
        check_error<-1+check_error
      }
      if(output_2==1){
        check_error2 <- 1 + check_error2 
        
      }
      if(output_2==2){
        check_error3<-1+check_error3
      }
    }else{
      check_error2<-1+check_error2
    
    #Out_4<-rbind(Out_4,output_4)
    Out_3<-rbind(Out_3,output_3)
    Out_1<-rbind(Out_1,output_1)
    # Out_5<-rbind(Out_5,output_5)
    # Out_6<-rbind(Out_6,output_6)
    # Out_7<-rbind(Out_7,output_7)
    # Out_8<-rbind(Out_8,output_8)
    }
    Out_3<-rbind(Out_3,output_3)
    Out_1<-rbind(Out_1,output_1)
    # Out_5<-rbind(Out_5,output_5)
    # Out_6<-rbind(Out_6,output_6)
    # Out_7<-rbind(Out_7,output_7)
    # Out_8<-rbind(Out_8,output_8)
    remove(output_3,output_1,output_2)
}

   if (sum(Out_3,na.rm = T) > 0){
    output_33<-sum(Out_3,na.rm = T)
  } else {
    output_33<- NaN
  }
  
  if (check_error==4){
    output_22<- 0
  } 
  if (check_error3==4){
    output_22<- 2
  } 
  if (check_error2 >=1){
    output_22<- 1
  } 
  if(nrow(Out_1) > 0){
    output_11<-Out_1
  } else {
    output_11<-to_walash
  }
  output<- list(output_11,output_22,output_33 )
  return(output)
  
}



