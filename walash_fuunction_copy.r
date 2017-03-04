walash_outlier <- function(to_walash ){
  
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
  data_out_NA <- na.omit(zzz)
  Q_identifier <- unique(zzz$quarter)
  Out_4<-data.frame()
  Out_3<-data.frame()
  Out_5<-data.frame()
  Out_6<-data.frame()
  Out_1<-data.frame()
  Out_7<-data.frame()
  Out_8<-data.frame()
  check_error<- 0
  check_error2<- 0
  check_error3<- 0
  for (kkk in Q_identifier){
    # kkk<- "Q1"
    zzz_Q<- zzz %>%
      na.omit(zzz$Value)%>%
      filter(quarter == kkk)
    zzz_Q_NA<- zzz %>%
      #na.omit(zzz$Value)%>%
      filter(quarter == kkk)
    #checks if the quarter have values or all of them are NA
    if (nrow(zzz_Q) == 0){
      output_1<- zzz_Q_NA
      output_2<- 0
      output_4<-0
      output_3<-0
      output_5<-0
      output_6<-0
      output_7<-0
      output_8<-0
    } 
    else {
      n=nrow(zzz_Q)
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
        #output_2<-2
        output_4<-0
        output_3<-0
        output_5<-0
        output_6<-0
        output_1<- zzz_Q_NA
        output_7<-0
        output_8<-0
        
      } else{
        # get the possible outliers from the timeseries
        box_objec<-boxplot(zzz_Q$Value)
        
        ind_no<- which(zzz_Q$Value %in% box_objec$out)
        pos_out<-data.frame()
        ind_no<-as.data.frame(ind_no)
        val<-box_objec$out
        pos_out<- cbind(ind_no,val)
        r_box<-nrow(pos_out)
        if (r_box >=1) {
          sort_ind <- order(zzz_Q$Value)
          sorted_data<-zzz_Q[sort_ind,]
          outliers_dat<- data.frame()
          plateau <- data.frame()
          pl_ss<- data.frame()
          pl_ll<-data.frame()
          x_ss<- data.frame()
          x_ll<- data.frame()
          x_Large_o<- -999999
          X_Small_o<- 999999
          for (r in 1:r_box){
           # r<-1
            c = ceiling(sqrt(2*n))
            k = r + c
            b_Squ = 1/Alpha
            a = (1+b_Squ*sqrt((c-b_Squ)/(c-1)))/(c-b_Squ-1)
            X_Small = sorted_data$Value[r] - (1+a)*sorted_data$Value[r+1] + a*sorted_data$Value[k]
            if (X_Small < 0 ){
              outliers_dat<-rbind(outliers_dat,sorted_data[r,])
              x_ss<- rbind(x_ss,X_Small)
            }
            if( X_Small == X_Small_o){
              plateau<- rbind(plateau,sorted_data[r,])
              pl_ss<- rbind(pl_ss,X_Small)
            }
            x_Large = sorted_data$Value[n+1-r] - (1+a)*sorted_data$Value[n-r] + a*sorted_data$Value[n+1-k]
            if (x_Large > 0 ){
              outliers_dat<-rbind(outliers_dat,sorted_data[r,])
              x_ll<- rbind(x_ll,x_Large)
            }
            if( x_Large_o ==x_Large){
              plateau<- rbind(plateau,sorted_data[r,])
              pl_ll<- rbind(pl_ll,x_Large)
            }
            X_Small_o=X_Small
            x_Large_o=x_Large
          }
          
          
          for (dave in 1:nrow(outliers_dat)){ 
            find_out<- outliers_dat$DateTime[dave]==zzz_Q_NA$DateTime
            ind_to_rep<-which(find_out)
            zzz_Q_NA$Value[ind_to_rep]= NA
          }
          for (fed in 1:nrow(plateau)){ 
            find_out_p<- plateau$DateTime[fed]==zzz_Q_NA$DateTime
            ind_to_rep_p<-which(find_out_p)
            zzz_Q_NA$Value[ind_to_rep_p]= NA
          }
          output_4<-r_box
          output_3<-nrow(outliers_dat)
          output_1<-zzz_Q_NA
          output_5<-nrow(x_ss)
          output_6<-nrow(x_ll)
          output_7<-nrow(pl_ss)
          output_8<-nrow(pl_ll)
        } else {
          output_2<-2
          output_4<-0
          output_3<-0
          output_5<-0
          output_6<-0
          output_1<- zzz_Q_NA
          output_7<-0
          output_8<-0
        }
        
      }
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
    }
    Out_4<-rbind(Out_4,output_4)
    Out_3<-rbind(Out_3,output_3)
    Out_1<-rbind(Out_1,output_1)
    Out_5<-rbind(Out_5,output_5)
    Out_6<-rbind(Out_6,output_6)
    Out_7<-rbind(Out_7,output_7)
    Out_8<-rbind(Out_8,output_8)
    remove(output_4,output_3,output_5,output_6,output_1,ind_to_rep,find_out,sorted_data,output_2,output_8,output_7 )
  } 
  if (sum(Out_7,na.rm = T) > 0){
    output_77<-sum(Out_7,na.rm = T)
  } else {
    output_77<- "no Small Plateau"
  }
  if (sum(Out_8,na.rm = T) > 0 ){
    output_88<-sum(Out_8,na.rm = T)
  } else {
    output_88<- "no Large Plateau"
  }
  if (sum(Out_4,na.rm = T) > 0){
    output_44<-sum(Out_4,na.rm = T)
  } else {
    output_44<- "no Suspected"
  }
  if (sum(Out_5,na.rm = T) > 0 ){
    output_55<-sum(Out_5,na.rm = T)
  } else {
    output_55<- "no small O."
  }
  if (sum(Out_6,na.rm = T) > 0){
    output_66<-sum(Out_6,na.rm = T)
  } else {
    output_66<- "no large O."
  }
  
  if (sum(Out_3,na.rm = T) > 0){
    output_33<-sum(Out_3,na.rm = T)
  } else {
    output_33<- NaN
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
  if(nrow(Out_1) > 0){
    output_11<-Out_1
  } else {
    output_11<-to_walash
  }
  output<- list(output_11,output_22,output_33,output_44,output_55,output_66,output_77,output_88 )
  return(output)

}

