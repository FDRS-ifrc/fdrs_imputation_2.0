#load packages
library(jsonlite)
library(data.table)
library(dplyr)
library(rlang)

#api-key

setwd(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\challenge_data_generation_script)")

key=fread("key.txt",header=FALSE)


#list of KPIs

kpi_list=c(
'KPI_Climate_CPD',
'KPI_ClimateHeat_CPD',
'KPI_ReachCTP_CPD',
'KPI_ReachDRER_CPD',
'KPI_ReachDRR_CPD',
'KPI_ReachH_CPD',
'KPI_ReachHI_CPD',
'KPI_ReachHPM_CPD',
'KPI_ReachL_CPD',
'KPI_ReachLTSPD_CPD',
'KPI_ReachM_CPD',
'KPI_ReachRCRCEd_CPD',
'KPI_ReachS_CPD',
'KPI_ReachSI_CPD',
'KPI_ReachWASH_CPD',
'KPI_TrainFA_Tot',
'KPI_DonBlood_Tot',
#'KPI_GB_Tot',
'KPI_PeopleVol_Tot',
'KPI_PStaff_Tot',
'KPI_noLocalUnits',
'KPI_IncomeLC_CHF',
'KPI_expenditureLC_CHF')
#'KPI_FirstSubmitDate',
#'KPI_NSFP_PublishDate',
#'KPI_NSFP_StartDate',
#'KPI_NSFP_SubmitDate',
#'KPI_NSFP_ValidationDate',
#'KPI_NSFP_WasPublished',
#'KPI_NSFP_WasStarted',
#'KPI_NSFP_WasSubmitted',
#'KPI_NSFP_WasValidated',
#'KPI_NSGS_PublishDate',
#'KPI_NSGS_StartDate',
#'KPI_NSGS_SubmitDate',
#'KPI_NSGS_ValidationDate',
#'KPI_NSGS_WasPublished',
#'KPI_NSGS_WasStarted',
#'KPI_NSGS_WasSubmitted',
#'KPI_NSGS_WasValidated',
#'KPI_NSR_PublishDate',
#'KPI_NSR_StartDate',
#'KPI_NSR_SubmitDate',
#'KPI_NSR_ValidationDate',
#'KPI_NSR_WasPublished',
#'KPI_NSR_WasStarted',
#'KPI_NSR_WasSubmitted',
#'KPI_NSR_WasValidated',
#'KPI_WasSubmitted',
#'validated'

data=c()
for(kpi in kpi_list){
  for(year in 2010:2022){
    print(paste(kpi,year))
    api=fromJSON(paste("https://data-api.ifrc.org/api/KpiImputedValue?kpicode=",kpi,"&year=",year,"&apiKey=",key,sep=""))
    if(length(api)>0){
      data=rbind.data.frame(data,cbind(year,kpi,api))
    }
  }
}


ns=fread(r"(C:\Users\dirk.merkhof\OneDrive - IFRC\Documents\git\fdrs_imputation_2.0\challenge_data\ns_data.csv)")
ns=ns %>% select(KPI_DON_code) %>% rename("doncode"="KPI_DON_code")

#add all national societies to all KPI's and years
data=ns %>% 
  cross_join(data %>% distinct(year)) %>% 
  cross_join(data %>% distinct(kpi)) %>% 
  left_join(data,by=c("doncode","kpi","year"))

for(year in 2010:2022){
  fwrite(data %>% filter(year==.env$year),paste("../challenge_data/challenge_data_",year,".csv",sep=""))
}

#for submissions date
data_submitted=c()
kpi='KPI_FirstSubmitDate'
for(year in 2022:2022){
  data_submitted=rbind.data.frame(data_submitted,cbind(year,fromJSON(paste("https://data-api.ifrc.org/api/KpiImputedValue?kpicode=",kpi,"&year=",year,"&apiKey=",key,sep=""))))
  #join
  data_jun=data %>% filter(year==.env$year) %>% 
    left_join(data_submitted,by=c("year","doncode")) %>% 
    mutate(value.y=strptime(value.y,"%Y-%m-%d")) %>% 
    filter(value.y<"2022-07-01") %>% 
    select(-value.y) %>% 
    rename(value=value.x)
  fwrite(data_jun,paste("../challenge_data/challenge_data_",year,"_jun.csv",sep=""))
}

#National Societies
fwrite(fromJSON(paste("https://data-api.ifrc.org/api/entities/ns?apikey=",key,sep="")),"../challenge_data/ns_data.csv")

#Indicators
fwrite(fromJSON(paste("https://data-api.ifrc.org/api/indicator?apikey=",key,sep="")),"../challenge_data/kpi_data.csv")


json=fromJSON("https://data-api.ifrc.org/api/data?indicator=KPI_firstsubmitdate&apiKey=21e401ae-6b35-404b-a72a-b74cce66dee3")
