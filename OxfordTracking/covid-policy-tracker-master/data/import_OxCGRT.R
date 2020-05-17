#q<-read.csv("OxCGRT_latest.csv")
#saveRDS(q,"Oxf.rds")

#see here: https://www.nber.org/papers/w27082 for paper
#and here for data: (https://osf.io/3sn2k/)
#I believe the data collection period was between 20/03 and 08/04. Or at least the one that they made available online and that I am currently working with.
#They append data on government policy from Oxford:  https://www.bsg.ox.ac.uk/research/publications/variation-government-responses-covid-19 


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