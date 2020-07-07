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
  left_join(lockdown[c("Country", "DateLockDown")],by = "Country") %>%
  mutate(Date=as.Date(Date,format="%d/%m/%y"),
         DateLockDown = as.Date(DateLockDown,format="%d/%m/%y")) %>%
  # mutate(temperatureBeforeLockdown = )
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
