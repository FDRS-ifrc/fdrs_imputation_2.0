#load packages
library(jsonlite)
library(data.table)
library(dplyr)
library(rlang)

#api-key

key=fread("key.txt",header=FALSE)


#list of KPIs

kpi_list=c(
'KPI_Climate_CPD_IP',
'KPI_ClimateHeat_CPD_IP',
'KPI_ReachCTP_CPD_IP',
'KPI_ReachDRER_CPD_IP',
'KPI_ReachDRR_CPD_IP',
'KPI_ReachH_CPD_IP',
'KPI_ReachHI_CPD_IP',
'KPI_ReachHPM_CPD_IP',
'KPI_ReachL_CPD_IP',
'KPI_ReachLTSPD_CPD_IP',
'KPI_ReachM_CPD_IP',
'KPI_ReachRCRCEd_CPD_IP',
'KPI_ReachS_CPD_IP',
'KPI_ReachSI_CPD_IP',
'KPI_ReachWASH_CPD_IP',
'KPI_TrainFA_Tot_IP',
'KPI_DonBlood_Tot_IP',
'KPI_GB_Tot_IP',
'KPI_PeopleVol_Tot_IP',
'KPI_PStaff_Tot_IP',
'KPI_noLocalUnits_IP',
'KPI_IncomeLC_CHF_IP',
'KPI_expenditureLC_CHF_IP')
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

for(year in 2010:2022){
  fwrite(data %>% filter(source!="I",year==.env$year) %>% select(-source) %>% mutate(kpi=gsub("_IP","",kpi)),paste("../challenge_data_",year,".csv",sep=""))
}

#for submissions date
data_submitted=c()
kpi='KPI_FirstSubmitDate'
for(year in 2021:2021){
  data_submitted=rbind.data.frame(data_submitted,cbind(year,fromJSON(paste("https://data-api.ifrc.org/api/KpiImputedValue?kpicode=",kpi,"&year=",year,"&apiKey=",key,sep=""))))
  #join
  data_jun=data %>% filter(source!="I",year==.env$year) %>% 
    select(-source) %>% 
    left_join(data_submitted,by=c("year","doncode")) %>% 
    mutate(value.y=strptime(value.y,"%Y-%m-%d")) %>% 
    filter(value.y<"2022-07-01") %>% 
    select(-value.y) %>% 
    rename(value=value.x)
  fwrite(data_jun,paste("../challenge_data_",year,"_jun.csv",sep=""))
}
