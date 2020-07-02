# Identify date of first reported infections & death
#source for owid-covid-data.csv: https://ourworldindata.org/coronavirus-source-data 
#We don't need to update this regularly for the first cases and deaths but this is also the data
#where we draw the total cases and deaths from. So prob. good to update once a week or so.
days<-read.csv(here::here("CasesDeaths/owid-covid-data.csv"))%>% 
  rename(Country=location)%>%
  mutate(Country = recode(Country, "Timor" = "Timor Leste")) %>%
  mutate(Country=countrycode(Country,'country.name','country.name')) %>%
  mutate(Date=as.Date(date))
  #filter(Date!="2020-12-31")%>%
 # mutate(DeathsBeforeGoogle=case_when(Date=="2020-03-19"~total_deaths)) %>%
 # left_join(lockdown[c("Country", "DateLockDown")], by = "Country") %>%
#  mutate(DateLockDown = as.Date(DateLockDown, format = "%d/%m/%Y")) %>%
 # mutate(DaysSinceLockdown = difftime(Date,DateLockDown,
  #                                    units = "days")) 

 #q<-days%>%
  # group_by(Country)%>%
   #summarise(m=last(total_deaths))
# 
# # Identify date of first reported infections & death
# 
# days_1case<-days[days$total_cases>0,]%>%
#   group_by(Country)%>%
#   summarise(Date_1Confirmed=first(Date))
# 
# days_1death<-days[days$total_deaths>0,]%>%
#   group_by(Country)%>%
#   summarise(Date_1Death=first(Date))
# 
# # Merge into sub-dataset of cases, deaths, and timing
# 
# time_long<-merge(merge(
#   days,
#   days_1case,by="Country",all=T),
#   days_1death,by="Country",all=T)
# 
# time_short<-time_long%>%
#   group_by(Country)%>%
#   arrange(Country,Date)%>%
#   summarise(Date_1confirmed=first(Date_1Confirmed),Date_1death=first(Date_1Death),
#             TotalCases=last(total_cases),TotalDeaths=last(total_deaths),Google=mean(DeathsBeforeGoogle,na.rm=T))
# 
# # Compute time country has been in lockdown, since it had first case, death...
# # Note: NEEDS UPDATING (dates)
# 
# DaysLock_short<-merge(lockdown,time_short,by="Country",all=T)%>%
#   mutate(DateLockDown=as.Date(DateLockDown,format="%d/%m/%Y"))%>%
#   mutate(DaysDuration=ifelse(Type=="None",0,as.Date("29/03/2020",format="%d/%m/%y")-as.Date(DateLockDown)))%>%
#   mutate(DaysLockDownStart=DateLockDown-as.Date("22/01/2020",format="%d/%m/%Y"))%>%
#   mutate(Days_LockDown_1case=as.Date("29/03/2020",format="%d/%m/%y")-as.Date(Date_1confirmed,format="%d/%m/%y"))%>%
#   mutate(Days_LockDown_1death=as.Date("29/03/2020",format="%d/%m/%y")-as.Date(Date_1death,format="%d/%m/%y"))
