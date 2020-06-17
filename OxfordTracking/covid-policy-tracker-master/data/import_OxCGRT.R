q<-read.csv("OxfordTracking/covid-policy-tracker-master/data/OxCGRT_latest.csv")
saveRDS(q,"Oxf.rds")

#Data on government policy from Oxford:  https://www.bsg.ox.ac.uk/research/publications/variation-government-responses-covid-19 


Ox<-readRDS("OxfordTracking/covid-policy-tracker-master/data/Oxf.rds")%>%
  rename(Country=CountryName)%>%
  mutate(Country=countrycode::countrycode(Country,"country.name","country.name"))%>%
  mutate(Date=lubridate::ymd(Date))%>%
  mutate(week=lubridate::floor_date(Date,"week"))

  #mutate_at(vars(beh_stayhome:weight_sample),.funs=as.numeric)



#source("Google/import_mobility.R")


#OxCGRT<-merge(mobility_long,Ox,by=c("Country","Date"),all=T)%>%
  #arrange(Country,Date)

#df<- Reduce(function(...) full_join(..., by=c('Country','Date'), datasets.to.merge.long))

#df<-merge(mobility_long,Ox,by=)