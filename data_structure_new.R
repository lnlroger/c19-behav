library("tidyverse")
library("countrycode")

# Import google mobility data ----

# mobility_long: daily data
# mobility_short: mobility on latest available date

mobility_long <- read.csv("Google/Global_Mobility_Report.csv")%>%
  filter(sub_region_1=="")%>%
  mutate(Movement=rowMeans(.[,c("retail_and_recreation_percent_change_from_baseline",
                                "grocery_and_pharmacy_percent_change_from_baseline",
                                "parks_percent_change_from_baseline", 
                                "transit_stations_percent_change_from_baseline",
                                "workplaces_percent_change_from_baseline")],na.rm=T))%>%
  mutate(Date=as.Date(date))%>%
  mutate(Country=countrycode(country_region_code,'iso2c','country.name')) %>%
  mutate(Country=replace(Country, is.na(country_region_code), 'Namibia'))
  

mobility_short<-mobility_long%>%
  filter(Date==as.Date("2020-04-11"))


# Lockdown dates ----

#lockdown has been customised so as to take the median Province's attitude. 
  #The median is calculated by arranging by date of lockdown.
  #This approach coerces the US to the lockdown from its median state: N. Hampshire for example.
  #If you want to see the lock-down dates per state, see the "CountryLockdowndates.csv" instead.
lockdown<- read.csv("LockDown/countryLockdowndates_custom.csv") %>%
  rename(DateLockDown=Date)%>%
  mutate(Lock=ifelse(Type=="None","No","Yes"))%>%
  mutate(Country=countrycode(Country,'country.name','country.name'))
  
#2020-01-22 is the date from which the covid dataset starts counting



# Identify date of first reported infections & death
#source for owid-covid-data.csv: https://ourworldindata.org/coronavirus-source-data 
#We don't need to update this regularly for the first cases and deaths but this is also the data
#where we draw the total cases and deaths from. So prob. good to update once a week or so.
days<-read.csv(here::here("CasesDeaths/owid-covid-data.csv"))%>% 
  rename(Country=location)%>%
  mutate(Country = recode(Country, "Timor" = "Timor Leste")) %>%
  mutate(Country=countrycode(Country,'country.name','country.name')) %>%
  mutate(Date=as.Date(date))%>%
  #filter(Date!="2020-12-31")%>%
  mutate(DeathsBeforeGoogle=case_when(Date=="2020-03-19"~total_deaths))


q<-days%>%
  group_by(Country)%>%
  summarise(m=mean(DeathsBeforeGoogle,na.rm=T))

# Identify date of first reported infections & death

days_1case<-days[days$total_cases>0,]%>%
  group_by(Country)%>%
  summarise(Date_1Confirmed=first(Date))

days_1death<-days[days$total_deaths>0,]%>%
  group_by(Country)%>%
  summarise(Date_1Death=first(Date))

# Merge into sub-dataset of cases, deaths, and timing

time_long<-merge(merge(
  days,
  days_1case,by="Country",all=T),
  days_1death,by="Country",all=T)

time_short<-time_long%>%
  group_by(Country)%>%
  arrange(Country,Date)%>%
  summarise(Date_1confirmed=first(Date_1Confirmed),Date_1death=first(Date_1Death),
            TotalCases=last(total_cases),TotalDeaths=last(total_deaths),Google=mean(DeathsBeforeGoogle,na.rm=T))
  
# Compute time country has been in lockdown, since it had first case, death...
# Note: NEEDS UPDATING (dates)

DaysLock_short<-merge(lockdown,time_short,by="Country",all=T)%>%
  mutate(DateLockDown=as.Date(DateLockDown,format="%d/%m/%Y"))%>%
  mutate(DaysDuration=ifelse(Type=="None",0,as.Date("29/03/2020",format="%d/%m/%y")-as.Date(DateLockDown)))%>%
  mutate(DaysLockDownStart=DateLockDown-as.Date("22/01/2020",format="%d/%m/%Y"))%>%
  mutate(Days_LockDown_1case=as.Date("29/03/2020",format="%d/%m/%y")-as.Date(Date_1confirmed,format="%d/%m/%y"))%>%
  mutate(Days_LockDown_1death=as.Date("29/03/2020",format="%d/%m/%y")-as.Date(Date_1death,format="%d/%m/%y"))
  


# Weather data
# Note: Before and after what? shouldn't it be date of lockdown, rather than 23/2 everywhere?

weather_short<-read.csv("Weather/covid_dataset.csv")%>%
  rename(Country=Country.Region)%>%
  mutate(Country=countrycode(Country,'country.name','country.name')) %>%
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

weather_long<-read.csv("Weather/covid_dataset.csv")%>%
  rename(Country=Country.Region)%>%
  mutate(Country=countrycode(Country,'country.name','country.name')) %>%
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

wvs<-read.csv("WVS/WVS_per_Country.csv")%>%
  #filter(Wave==6)%>%
  dplyr::select(Country,E235,E236,E124,E229,Wave)%>%
  mutate(Country=countrycode(Country,'country.name','country.name')) %>%
  rename(Democracy=E235,Democraticness=E236,Civil=E124,Opression=E229)%>%
  group_by(Country)%>%
  filter(Wave==max(Wave)) #I keep the values for each country with the most recent available wave


rol<-read.csv("WB/RuleOfLaw2018.csv")%>%
  mutate(Country=Country.Name,ROL=as.numeric(levels(X2018..YR2018.))[X2018..YR2018.])%>%
  mutate(Country=countrycode(Country,'country.name','country.name')) %>%
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


wb<-read.csv("WB/WorldBank.csv")%>%
  mutate(Country=countrycode(Country.Code,'wb','country.name'))%>%
  mutate(log_GDP_pc=log(GDP_pc))
  

wb<-merge(wb,read.csv("WB/WB_hosp_bed.csv"),by="Country.Code",all=T)          

# PolityIV index ----

polityIV <- read.csv("Politics/polity4_v2018.csv")%>%
  filter(year == 2018) %>%
  mutate(Country=countrycode(scode,'p4c','country.name')) %>%
  dplyr::select('Country','polity2')
  

# UN population data ----
UNpop <- read.csv("UN-Population/population_division_UN_Houseshold_Size_and_Composition_2019.csv") %>%
  rename(Country = �..Country) %>%
  mutate(Country = recode(Country, "Réunion" = "Reunion", "Saint-Barthélemy" = "Saint-Barthelemy")) %>%
  mutate(Country = countrycode(Country,"country.name","country.name")) %>%
  group_by(Country) %>%
  filter(date==last(date)) %>%
  distinct(Country,date)



# Elections ----
  
`%notin%` <- Negate(`%in%`)

elections<-read.csv("Politics/DPI2017_basefile_Jan2018.csv") %>%
  mutate(ifs = recode(ifs, "ROM" = "ROU", "TMP" = "TLS", "ZAR" = "COD")) %>%
  filter(ifs %notin% c("CSK", "DDR", "SUN", "YMD", "YSR", "0", "")) %>%
  mutate(Country=countrycode(ifs,'wb','country.name')) %>%
  arrange(Country,year) %>%
  group_by(Country)%>%
  summarise(ElectionYear=last(year),ElectionWin=last(percent1),Country.Code=first(ifs))%>%
  naniar::replace_with_na_at(.vars = c("ElectionYear","ElectionWin","Country.Code"),
                             condition = ~.x == -999)%>%
  drop_na()
  


 
#RolFluElec<-merge(RolFlu,elections,by="Country.Code",all=T)%>%
 # mutate(Country=Country.x)

# Social preferences

social_prefs<-read.csv("Briq/socialprefs.csv")%>%
  rename(Country=country)%>%
  mutate(Country=countrycode(Country,"country.name","country.name"))

# Basic country co-variates (source?)

countries<-read.csv("Countries/countries_custom.csv")%>%
  #naniar::replace_with_na_at(.vars!=c("Population","Service"),condition = ~.x == -999.000)%>%
  na_if(.,-999)%>%
  mutate(Country = recode(Country, "Central African Rep. " = "Central African Republic", 
                          "Virgin Islands " = "U.S. Virgin Islands")) %>%
  mutate(Country=countrycode(Country,"country.name","country.name"))






#Collectivism - Hofstede
hf<-read.csv("Collectivism/hofstede.csv")%>%
  mutate(Country=countrycode(Country,"country.name","country.name"))







# Merge into single dataframes ----

# Short version (pure cross-section)

df_short <- merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(
  mobility_short,
  DaysLock_short,by="Country",all=T),
  wvs,by="Country",all=T),
  weather_short,by="Country",all=T),
  wb,by="Country",all=T),
  elections,by="Country",all=T),
  rol,by="Country",all=T),
  social_prefs,by="Country",all=T),
  countries,by="Country",all=T),
  polityIV, by="Country",all=T),
  UNpop, by="Country",all=T),
  hf,by="Country",all=T)


df_short<-df_short%>%
  filter(!is.na(Country))

df_short$Death_pc<-df_short$TotalDeaths/df_short$Population
df_short$Confirmed_pc<-df_short$TotalCases/df_short$Population
df2<-subset(df_short,df_short$Province!="Faroe Islands")
df2$Log_Death_pc<-ifelse(df2$Death_pc>0,log(df2$Death_pc),NA)
df2$Google_pc<-df2$Google/df2$Population
df2$Log_Google_pc<-ifelse(df2$Google_pc>0,log(df2$Google_pc),NA)



#write.csv(df2,"01052020_short.csv")


# Long version (daily data)

df_long<-merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(
  mobility_weather_death,
  lockdown,by="Country",all=T),
  wvs,by="Country",all=T),
  wb,by="Country",all=T),
  elections,by="Country",all=T),
  rol,by="Country",all=T),
  social_prefs,by="Country",all=T),
  countries,by="Country",all=T),
  time_short,by="Country",all=T),
  polityIV,by="Country",all=T),
  UNpop,by="Country",all=T),
  hf,by="Country",all=T)

df_long<-df_long%>%
  filter(!is.na(Country))


df_long$Death_pc<-df_long$total_deaths/df_long$Population
df_long$Confirmed_pc<-df_long$total_cases/df_long$Population
df3<-subset(df_long,df_long$Province!="Faroe Islands")
df3$Log_Death_pc<-ifelse(df3$Death_pc>0,log(df3$Death_pc),NA)

df3$Google_pc<-df3$DeathsBeforeGoogle/df3$Population
df3$Log_Google_pc<-ifelse(df3$Google_pc>0,log(df3$Google_pc),NA)

#write.csv(df3,"01052020_long.csv")

