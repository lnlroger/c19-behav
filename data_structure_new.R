mobility <- read.csv(here::here("mobility_report_regions_1_2.csv"))%>%
  filter(Region=="Total")%>%
  #dplyr::select(-Residential)%>%
  filter(Date=="2020-03-29")%>%
  mutate(Movement=rowMeans(.[,c("Retail...recreation","Grocery...pharmacy","Parks","Transit.stations",
                                       "Workplaces")],na.rm=T)) %>% 
  arrange(Country,Date)%>%
  mutate(Country=recode_factor(Country,
                               'United States of America' = 'US','United Kingdom'='UK',
                               'United Arab Emirates'='UAE','Czech Republic'='Czechia',
                               'Bosnia and Herzegovina'='Bosnia','United States'='US',"USA"="US",
                               'Viet Nam'='Vietnam','Congo (Kinshasa)'='Congo',"Bosnia Herzegovina"="Bosnia",
                               "Taiwan, China"="Taiwan", "Venezuela, RB"="Venezuela","Korea, Rep."="South Korea",
                               "Gambia, The"="Gambia","Serbia and Montenegro"="Serbia","Great Britain"="UK",
                               "Macedonia"="North Macedonia","Bosnian Federation"="Bosnia","Taiwan*"="Taiwan"))
  
           
  
  
 







#lockdown has been customised so as to take the median Province's attitude
lockdown<- read.csv(here::here("countryLockdowndates_custom.csv"))%>%
  rename(DateMovement=Date)%>%
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
  mutate(date=as.Date(date,format="%d/%m/%y"))%>%
  filter(date!="2020-12-31")%>%
  mutate(DeathsBeforeGoogle=case_when(date=="2020-03-19"~total_deaths))

days_1case<-days[days$new_cases==1,]%>%
  group_by(Country)%>%
  summarise(Date_1Confirmed=first(date))

days_1death<-days[days$new_deaths==1,]%>%
  group_by(Country)%>%
  summarise(Date_1Death=first(date))



time<-merge(merge(
  days,
  days_1case,by="Country",all=T),
  days_1death,by="Country",all=T)

time_short<-time%>%
  
  group_by(Country)%>%
  arrange(Country,date)%>%
  summarise(Date_1confirmed=first(Date_1Confirmed),Date_1death=first(Date_1Death),
            TotalCases=last(total_cases),TotalDeaths=last(total_deaths),Google=first(na.omit(DeathsBeforeGoogle)))
  

DaysLock<-merge(lockdown,time_short,by="Country",all=T)%>%
  mutate(DateMovement=as.Date(DateMovement,format="%d/%m/%Y"))%>%
  mutate(DaysDuration=ifelse(Type=="None",0,as.Date("29/03/2020",format="%d/%m/%y")-as.Date(DateMovement)))%>%
  mutate(DaysMovementStart=DateMovement-as.Date("22/01/2020",format="%d/%m/%Y"))%>%
  mutate(Days_Movement_1case=as.Date("29/03/2020",format="%d/%m/%y")-as.Date(Date_1confirmed,format="%d/%m/%y"))%>%
  mutate(Days_Movement_1death=as.Date("29/03/2020",format="%d/%m/%y")-as.Date(Date_1death,format="%d/%m/%y"))
  



weather<-read.csv("covid_dataset.csv")%>%
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



df<-merge(merge(merge(merge(merge(merge(merge(merge(
  mobility,
  DaysLock,by="Country",all=T),
  wvs,by="Country",all=T),
  weather,by="Country",all=T),
  wb,by="Country",all=T),
  elections,by="Country",all=T),
  rol,by="Country",all=T),
  social_prefs,by="Country",all=T),
  countries,by="Country",all=T)

df$Death_pc<-df$TotalDeaths/df$Population
df$Confirmed_pc<-df$TotalCases/df$Population
df2<-subset(df,df$Province!="Faroe Islands")
df2$Log_Death_pc<-ifelse(df2$Death_pc>0,log(df2$Death_pc),NA)
df2$Google_pc<-df2$Google/df2$Population
df2$Log_Google_pc<-ifelse(df2$Google_pc>0,log(df2$Google_pc),NA)




#write.csv(df2,"18042020.csv")


#countries<-read.csv("countries.csv")

#q<-semi_join(df,countries,by="Country")

#q$Continent<-countries$Continent
#q$Popular<-countries$Popular
#write.csv(q,"countries2.csv")





cor.test(as.numeric(df$DaysRestr),df$ROL,na.rm=TRUE)
cor.test(as.numeric(df$DaysRestr),df$Democracy)
cor.test(as.numeric(df$DaysRestr),df$Civil)
cor.test(as.numeric(df$DaysRestr),df$Democraticness)
cor.test(as.numeric(df$DaysRestr),df$Opression)

cor.test(as.numeric(df$Movement),df$ROL,na.rm=TRUE)
cor.test(as.numeric(df$Movement),df$Democracy)
cor.test(as.numeric(df$Movement),df$Civil)
cor.test(as.numeric(df$Movement),df$Democraticness)
cor.test(as.numeric(df$Movement),df$Opression)

cor.test(as.numeric(df$Days_1case),df$ROL)
cor.test(as.numeric(df$Days_1case),df$Democracy)
cor.test(as.numeric(df$Days_1case),df$Civil)

cor.test(as.numeric(df$Strict),df$ROL)
cor.test(as.numeric(df$Strict),df$Democracy)
cor.test(as.numeric(df$Strict),df$Civil)


cor.test(as.numeric(df$Days_pandemic),df$ROL)
cor.test(as.numeric(df$Days_pandemic),df$Democracy)
cor.test(as.numeric(df$Days_pandemic),df$Civil)

cor.test(as.numeric(df$Days_1death),df$ROL)
cor.test(as.numeric(df$Days_1death),df$Democracy)
cor.test(as.numeric(df$Days_1death),df$Civil)

cor.test(as.numeric(df$Days_1case),df$ROL)
cor.test(as.numeric(df$Days_1case),df$Democracy)
cor.test(as.numeric(df$Days_1case),df$Civil)



cor.test(df$Movement,df$ROL)
cor.test(df$RetailRecreation,df$ROL)
cor.test(df$GroceryPharmacy,df$ROL)
cor.test(df$Parks,df$ROL)
cor.test(df$TransitStations,df$ROL)
cor.test(df$Workplace,df$ROL)
cor.test(df$Residential,df$ROL)

df_restr<-filter(df,df$Reaction==1|df$Reaction==2)
df_norestr<-filter(df,df$Reaction==0)
df_NA<-filter(df,is.na(df$Reaction))

cor.test(df_NA$Movement,df_NA$ROL)

