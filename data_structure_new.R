mobility_long <- read.csv(here::here("Global_Mobility_Report.csv"))%>%
  rename(Country=country_region)%>%
  filter(sub_region_1=="")%>%
  mutate(Movement=rowMeans(.[,c("retail_and_recreation_percent_change_from_baseline","grocery_and_pharmacy_percent_change_from_baseline",
                                "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline",
                                "workplaces_percent_change_from_baseline")],na.rm=T))%>%
  mutate(Date=as.Date(date))%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))





mobility_short<-mobility_long%>%
  filter(Date==as.Date("2020-04-11"))
  



 







#lockdown has been customised so as to take the median Province's attitude. 
  #The median is calculated by arranging by date of lockdown.
  #This approach coerces the US to the lockdown from its median state: N. Hampshire for example.
  #If you want to see the lock-down dates per state, see the "CountryLockdowndates.csv" instead.
lockdown<- read.csv(here::here("countryLockdowndates_custom.csv"))%>%
  rename(DateLockDown=Date)%>%
  mutate(Lock=ifelse(Type=="None","No","Yes"))%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))
#2020-01-22 is the date from which the covid dataset starts counting

days<-read.csv("CovidDeaths.csv")%>%
  rename(Country=location)%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))%>%
  mutate(Date=as.Date(date,format="%d/%m/%y"))%>%
  filter(Date!="2020-12-31")%>%
  mutate(DeathsBeforeGoogle=case_when(Date=="2020-03-19"~total_deaths))


days_1case<-days[days$new_cases==1,]%>%
  group_by(Country)%>%
  summarise(Date_1Confirmed=first(Date))

days_1death<-days[days$new_deaths==1,]%>%
  group_by(Country)%>%
  summarise(Date_1Death=first(Date))



time_long<-merge(merge(
  days,
  days_1case,by="Country",all=T),
  days_1death,by="Country",all=T)

time_short<-time_long%>%
  
  group_by(Country)%>%
  arrange(Country,Date)%>%
  summarise(Date_1confirmed=first(Date_1Confirmed),Date_1death=first(Date_1Death),
            TotalCases=last(total_cases),TotalDeaths=last(total_deaths),Google=first(na.omit(DeathsBeforeGoogle)))
  

DaysLock_short<-merge(lockdown,time_short,by="Country",all=T)%>%
  mutate(DateLockDown=as.Date(DateLockDown,format="%d/%m/%Y"))%>%
  mutate(DaysDuration=ifelse(Type=="None",0,as.Date("29/03/2020",format="%d/%m/%y")-as.Date(DateLockDown)))%>%
  mutate(DaysLockDownStart=DateLockDown-as.Date("22/01/2020",format="%d/%m/%Y"))%>%
  mutate(Days_LockDown_1case=as.Date("29/03/2020",format="%d/%m/%y")-as.Date(Date_1confirmed,format="%d/%m/%y"))%>%
  mutate(Days_LockDown_1death=as.Date("29/03/2020",format="%d/%m/%y")-as.Date(Date_1death,format="%d/%m/%y"))
  



weather_short<-read.csv("covid_dataset.csv")%>%
  rename(Country=Country.Region)%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))%>%
  mutate(Date=as.Date(Date,format="%d/%m/%y"))%>%
  mutate(temperatureBefore=case_when(Date<as.Date("2020-02-23")~temperature))%>%
  mutate(temperatureAfter=case_when(Date>=as.Date("2020-02-23")~temperature))%>%
  mutate(humidityBefore=case_when(Date<as.Date("2020-02-23")~humidity))%>%
  mutate(humidityAfter=case_when(Date>=as.Date("2020-02-23")~humidity))%>%
  group_by(Country)%>%
  summarise(Temperature=mean(temperature,na.rm=TRUE),
            TemperatureBfr=mean(temperatureBefore,na.rm=T),TemperatureAftr=mean(temperatureAfter,na.rm=T),
            TemperatureDif=TemperatureAftr-TemperatureBfr,
            Humidity=mean(humidity,na.rm=T), 
            HumidityBfr=mean(humidityBefore,na.rm=TRUE),HumidityAftr=mean(humidityAfter,na.rm=T),
            HumidityDif=HumidityAftr-HumidityBfr)%>%
  naniar::replace_with_na_at(.vars = c("Temperature","Humidity"),
                             condition = ~.x == -999)




weather_long<-read.csv("covid_dataset.csv")%>%
  rename(Country=Country.Region)%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))%>%
  mutate(Date=as.Date(Date,format="%d/%m/%y"))%>%
  mutate(temperatureBefore=case_when(Date<as.Date("2020-02-23")~temperature))%>%
  mutate(temperatureAfter=case_when(Date>=as.Date("2020-02-23")~temperature))%>%
  mutate(humidityBefore=case_when(Date<as.Date("2020-02-23")~humidity))%>%
  mutate(humidityAfter=case_when(Date>=as.Date("2020-02-23")~humidity))%>%
  naniar::replace_with_na_at(.vars = c("Temperature","Humidity"),
                             condition = ~.x == -999)%>%
  group_by(Country,Date)%>%
  summarise(Temperature=mean(temperature,na.rm=TRUE),
            TemperatureBfr=mean(temperatureBefore,na.rm=T),TemperatureAftr=mean(temperatureAfter,na.rm=T),
            Humidity=mean(humidity,na.rm=T), 
            HumidityBfr=mean(humidityBefore,na.rm=TRUE),HumidityAftr=mean(humidityAfter,na.rm=T))






mobility_weather_death<-merge(merge(
  mobility_long,
  weather_long,by=c("Country","Date"),all=T),
  time_long,by=c("Country","Date"),all=T)




#write.csv(Time,"Time.csv")

wvs<-read.csv(here::here("WVS_per_Country.csv"))%>%
  #filter(Wave==6)%>%
  dplyr::select(Country,E235,E236,E124,E229,Wave)%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))%>%
  rename(Democracy=E235,Democraticness=E236,Civil=E124,Opression=E229)%>%
  group_by(Country)%>%
  filter(Wave==max(Wave)) #I keep the values for each country with the most recent available wave


rol<-read.csv(here::here("RuleOfLaw2018.csv"))%>%
  mutate(Country=Country.Name,ROL=as.numeric(levels(X2018..YR2018.))[X2018..YR2018.])%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))%>%
  dplyr::select(Country,Country.Code,ROL)%>%
  mutate(Country.Code=recode_factor(Country.Code,
                               'ROM'="ROU"))

#flu<-read.csv("influenza-vaccination-rates.csv")%>%
  #group_by(Country.Code)%>%
  #summarise(Time=last(time),VaccinationRate=last(value))%>%
  #mutate(OECE=1)
  

#RolFlu<-merge(rol,flu,by="Country.Code",all=T)

#countries<-read.csv(here::here("Countries.csv"))%>%
  #mutate(Country=trimws(Country))%>%
 # mutate(Population:Service=as.numberic(Population:Service))

#WORLD BANK
# wb<-merge(merge(merge(merge(
#                 read.csv("WB_GDP_pc.csv"),
#                 read.csv("WB_unemployment.csv"),by="Country.Code",all=T),
#                 read.csv("WB_communicable.csv"),by="Country.Code",all=T),
#                 read.csv("WB_1564.csv"),by="Country.Code",all=T),
#                 read.csv("WB_65up.csv"),by="Country.Code",all=T)

#write.csv(wb,"WorldBank.csv")

wb<-read.csv("WorldBank.csv")%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))%>%
  mutate(log_GDP_pc=log(GDP_pc))
  

wb<-merge(wb,read.csv("WB_hosp_bed.csv"),by="Country.Code",all=T)          


  

elections<-read.csv("DPI2017_basefile_Jan2018.csv")%>%
  mutate(Country=ï..countryname)%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))%>%
  #mutate(ifs="Country.Code")%>%
  arrange(Country,year)%>%
  group_by(Country)%>%
  summarise(ElectionYear=last(year),ElectionWin=last(percent1),Country.Code=first(ifs))%>%
  naniar::replace_with_na_at(.vars = c("ElectionYear","ElectionWin","Country.Code"),
                             condition = ~.x == -999)%>%
  drop_na()
  
 
#RolFluElec<-merge(RolFlu,elections,by="Country.Code",all=T)%>%
 # mutate(Country=Country.x)

# dplyr::select(Country,percent1)

social_prefs<-read.csv("socialprefs.csv")%>%
  rename(Country=country)%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))


countries<-read.csv("countries_custom.csv")%>%
  #naniar::replace_with_na_at(.vars!=c("Population","Service"),condition = ~.x == -999.000)%>%
  na_if(.,-999)%>%
  mutate(Country=trimws(Country))%>%
  #mutate_at(vars(Population:Service),funs(as.numeric))%>%
  
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))



df_short<-merge(merge(merge(merge(merge(merge(merge(merge(
  mobility_short,
  DaysLock_short,by="Country",all=T),
  wvs,by="Country",all=T),
  weather_short,by="Country",all=T),
  wb,by="Country",all=T),
  elections,by="Country",all=T),
  rol,by="Country",all=T),
  social_prefs,by="Country",all=T),
  countries,by="Country",all=T)


df_short$Death_pc<-df_short$TotalDeaths/df_short$Population
df_short$Confirmed_pc<-df_short$TotalCases/df_short$Population
df2<-subset(df_short,df_short$Province!="Faroe Islands")
df2$Log_Death_pc<-ifelse(df2$Death_pc>0,log(df2$Death_pc),NA)
df2$Google_pc<-df2$Google/df2$Population
df2$Log_Google_pc<-ifelse(df2$Google_pc>0,log(df2$Google_pc),NA)

#write.csv(df2,"22042020_short.csv")



df_long<-merge(merge(merge(merge(merge(merge(merge(merge(
  mobility_weather_death,
  lockdown,by="Country",all=T),
  wvs,by="Country",all=T),
  wb,by="Country",all=T),
  elections,by="Country",all=T),
  rol,by="Country",all=T),
  social_prefs,by="Country",all=T),
  countries,by="Country",all=T),
  time_short,by="Country",all=T)


df_long$Death_pc<-df_long$total_deaths/df_long$Population
df_long$Confirmed_pc<-df_long$total_cases/df_long$Population
df3<-subset(df_long,df_long$Province!="Faroe Islands")
df3$Log_Death_pc<-ifelse(df3$Death_pc>0,log(df3$Death_pc),NA)

df3$Google_pc<-df3$DeathsBeforeGoogle/df3$Population
df3$Log_Google_pc<-ifelse(df3$Google_pc>0,log(df3$Google_pc),NA)

#write.csv(df3,"22042020_long.csv")

