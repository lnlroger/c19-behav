df<-read.csv(here("df_covid_long.csv"))%>%
  #filter(sub_region_1=="")%>%
  filter(Country=="United Kingdom")%>%
  filter(!is.na(StringencyIndex))%>%
  mutate(Movement_lag_1d=dplyr::lag(Movement,1))%>%
  mutate(Movement_lead_1d=dplyr::lead(Movement,1))%>%
  mutate(DifMove=Movement-Movement_lag_1d)%>%
  mutate(DifMoveLag=dplyr::lag(DifMove,1))%>%
  mutate(DifPol=StringencyIndex-dplyr::lag(StringencyIndex,1))



df$ID<-1:nrow(df)


OxVars<-c('C1_School.closing',  'C2_Workplace.closing',
          'C3_Cancel.public.events','C4_Restrictions.on.gatherings',
          'C5_Close.public.transport','C6_Stay.at.home.requirements',
          'C7_Restrictions.on.internal.movement','C8_International.travel.controls','H1_Public.information.campaigns')

OxFlags<-c('C1_Flag','C2_Flag','C3_Flag','C4_Flag','C5_Flag','C6_Flag','C7_Flag','H1_Flag')

GoogleVars<-c('retail_and_recreation_percent_change_from_baseline', 'grocery_and_pharmacy_percent_change_from_baseline', 'parks_percent_change_from_baseline', 
              'transit_stations_percent_change_from_baseline', 'workplaces_percent_change_from_baseline','residential_percent_change_from_baseline','Movement')


dfl<-reshape(df, idvar="ID",
             varying = list(c(GoogleVars,OxVars)),
             
             v.names = 'Value', 
             timevar='Type',
             times=c(GoogleVars,OxVars),
             
             direction = "long")%>%
  # dplyr::select(Country,Continent,Date,Value,Type)%>%
  mutate(Date=ymd(Date))




k<-df%>%
  dplyr::select(ID, StringencyIndex)

dfl<-merge(dfl,k,by="ID")%>%
  arrange(Country,Date,Type)%>%
  mutate(Value_lag_1d=dplyr::lag(Value,1))






