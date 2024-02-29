#loading data

#install.packages("data.table")
library(data.table)
library(dplyr)

#load 3 years
data19=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\challenge_data\challenge_data_2019.csv)")
data20=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\challenge_data\challenge_data_2020.csv)")
data21=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\challenge_data\challenge_data_2021.csv)")

#


#combine 3 years
data=rbind.data.frame(data19,data20,data21)

#The FDRS way

#Gov and Finance
data_submission=data %>% filter(year==2021) %>% select(-year)

#Reach
data_submission=rbind.data.frame(data_submission,
                                 data %>% 
                                   filter(year>=2019) %>% 
                                   select(-year) %>% 
                                   group_by(doncode,kpi) %>% 
                                   summarise(value=mean(value,na.rm=TRUE),.groups="drop")
                                 )

fwrite(data_submission,r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\Dirk\prediction_22.csv)")

#real values - - - - --  S U B M I S S I O N 

#F O R M A T 

#doncode TEXT, kpi TEXT, value VALUE




#Expected format: csv with doncode - kpi - value
submission=function(data_submission){
  data22=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\challenge_data\challenge_data_2022.csv)")
  
  #remove NAs
  data22=data22 %>% filter(!is.na(value)) %>% rename("real_value"="value")
  data_submission=data_submission %>% filter(!is.na(value))%>% rename("submission_value"="value")
  
  #left join with result
  
  join=data22 %>% left_join(data_submission,by=c("doncode","kpi"))
  
  #count missing joins percentage
  na=sum(is.na(join$submission_value))
  join=join%>% filter(!is.na(submission_value))
  
  #R2
  
  model = lm(real_value~submission_value, data=join)
  summary(model)$r.squared
  
  #MAPE
  mean(ifelse(abs(join$real_value-join$submission_value)/join$real_value==Inf,100,abs(join$real_value-join$submission_value)/join$real_value),na.rm=TRUE)
  
}



data22=data22 %>% left_join(data22_prediction,by=c("doncode"))
