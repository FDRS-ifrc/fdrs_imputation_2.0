#loading data

#install.packages("data.table")
library(data.table)
library(dplyr)

#load 3 years
data19=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\challenge_data\challenge_data_2019.csv)")
data20=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\challenge_data\challenge_data_2020.csv)")
data21=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\challenge_data\challenge_data_2021.csv)")

#combine 3 years
data=rbind.data.frame(data19,data20,data21)

#filter on one KPI
data=data %>% filter(kpi=="KPI_PeopleVol_Tot")

#data per year

data %>% group_by(year) %>% count()

#count missing values
data %>% group_by(year) %>% filter(is.na(value)) %>% count()

#Total Volunteers per year

data %>% group_by(year) %>% summarise(value=sum(value,na.rm=TRUE))


#simple forecasting regression
summary(data$value)

