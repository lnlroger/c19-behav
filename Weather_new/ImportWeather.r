#weather<-read.csv("Weather_new/WeatherJan-Jun-2020.csv")
#saveRDS(weather,file="weather.rds")
weather<-readRDS("Weather_new/weather.rds")%>%
  mutate(Lat_min_1=LATITUDE-0.1,
          Lat_max_1=LATITUDE+0.1,
          Lon_min_1=LONGITUDE-0.1,
          Lon_max_1=LONGITUDE+0.1)%>%
  mutate(Date=ymd(DATE))%>%
  mutate(Celcius=(TEMP-32)*5/9)

###Geocoding stations-reverse?


