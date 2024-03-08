#loading data

#install.packages("data.table")
library(data.table)
library(dplyr)
options(scipen = 50)

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



data_submission=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\Dirk\prediction_22.csv)")
data_submission=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\Baldur\Baldur_baseline_submission_BayesianRidge.csv)")
#Expected format: csv with doncode - kpi - value
submission=function(data_submission){
  data22=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\challenge_data\challenge_data_2022.csv)")
  
  #remove NAs and zeros
  data22=data22 %>% filter(!is.na(value),value!=0) %>% rename("real_value"="value")
  data_submission=data_submission %>% filter(!is.na(value))%>% rename("submission_value"="value")
  
  #left join with result
  
  join=data22 %>% left_join(data_submission,by=c("doncode","kpi"))%>% mutate(submission_value=round(submission_value,0))
  
  #count missing joins percentage
  missing=join %>% group_by(kpi) %>% summarise(na=sum(is.na(submission_value),is.na=TRUE))
  join=join%>% filter(!is.na(submission_value))
  
  #R2
  for(kpi_name in join %>% distinct(kpi) %>% pull(kpi)){
    model = lm(real_value~submission_value, data=join %>% filter(kpi==kpi_name))
    r2=summary(model)$r.squared
  
    #MAPE
    mape=join%>% filter(kpi==kpi_name) %>% summarise(mape=mean(abs(real_value-submission_value)/real_value)) %>% pull(mape)
    print(paste0(kpi_name,": R2 = ",r2," and MAPE = ",mape,", missing imputations = ",missing %>% filter(kpi==kpi_name) %>% pull(na),"/",join%>% filter(kpi==kpi_name) %>% nrow()))
  }
}



data22=data22 %>% left_join(data22_prediction,by=c("doncode"))
