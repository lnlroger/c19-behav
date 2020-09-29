#Source: https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-day?pageSize=100&dataTypes=%5B%22DEWP%22,%22FRSHTT%22,%22GUST%22,%22MAX%22,%22MIN%22,%22MXSPD%22,%22PRCP%22,%22SLP%22,%22SNDP%22,%22STP%22,%22TEMP%22,%22VISIB%22,%22WDSP%22%5D&bbox=%5B%2239.266,22.500,36.880,25.357%22%5D&startDate=%5B%222019-12-01T00:00:00%22%5D&endDate=%5B%222020-01-01T23:59:59%22%5D
library(lubridate)
library(tidyverse)
library(countrycode)
library(fuzzyjoin)
library(ggmap)

#Set wd in file location
#Load WeatherJan-Jun-2020.rdata


source("Weather_new/ImportWeather.r")
source("Google/import_mobility.R")
source("Briq/import_social-prefs.r")
source("Cities/ImportCities.r")
#source("ImportLong.r")




#Goal 1: merge google_cities with gps_cities while minimising data loss
#we now have east and west (lon) and north and south (lat). These are ranges for longitude and latitude. 
#Test: the merged file should have 53 countries. This is the intersection of Google_city and GPS at the country level. 
#To make matching easier, I start by creating a subset of google's mobility_regional that uses only the intersection with GPS. 

interGoogle_GPS<-short_mob%>%
  filter(Country %in% gps_coord$Country)%>%
  group_by(Country)

n_groups(interGoogle_GPS) #Good, back to 53, though, we prob. don't have city-level info for Croatia's GPS.

interGPS_Google<-gps_coord%>%
  filter(Country %in% mobility_regional$Country)%>%
  group_by(Country)

n_groups(interGPS_Google)


# US<-interGPS_Google%>%
#   group_by(Country,City)%>%
#   summarise(n=n())%>%
#   filter(Country=="United States")
# 
# US1<-interGoogle_GPS%>%
#   group_by(Country,City)%>%
#   summarise(n=n())%>%
#   filter(Country=="United States")

#South is the city's min lat, North is the city's max lat. West is min long and East max long. 
#We are doing a fuzzy_inner_join taking the lat from Google and placing it between the min and max from gps_coord & same for longitude
#The ranges are pretty big for some cities, so we might end up with multiples... Not sure how these will be treated.

GpsGoo<-
  fuzzy_inner_join(interGPS_Google, interGoogle_GPS, 
                   by=c("lat"="SouthHalf", "lat"="NorthHalf","lon"="WestHalf","lon"="EastHalf"),
                   match_fun=list(`>=`, `<=`,`>=`,`<=`)) 

# US3<-GpsGoo%>%
#   group_by(Country.x,City.x)%>%
#   summarise(n=n())%>%
#   filter(Country.x=="United States")

#Gets rid of double entries
GpsGoog<-GpsGoo%>%
  group_by(lat,lon)%>%
  summarise_all(first)%>%
  ungroup()



GpsGoog$match<-ifelse(GpsGoog$Country.x==GpsGoog$Country.y,1,0)
lost<-GpsGoog%>%
  filter(match==0)

#3 cities are country-mismatched - we get rid of them and unify Country.x and Country.y
#I also rid of north, east,west,south and keep North, East, West, South - they are the same just short_mob used capitalisation. 


GpsGoog<-GpsGoog%>%
  filter(match==1)%>%
  mutate(country=Country.x)%>%
  dplyr::select(-Country.x,-Country.y,-north,-south,-east,-west)%>%
  group_by(country)


n_groups(GpsGoo)


#xx<-anti_join(interGPS_Google, interGoogle_GPS, by="Country")
# `%notin%` <- Negate(`%in%`)
# xx<-GpsGoog$Country.x%notin%interGoogle_GPS$Country









# Problem: locations like "East Middlands" from GPS, do not have coordinates because Google does not recognise them. 
# Fixed it manually in import_mobility.r
# Let's list all those with NA in lon or lat


# missing<-interGPS_Google%>%
#   filter(is.na(lat))

#Nothing is missing anymore..Good!




#Goal 2: add weather for every city

#We will use the short_weather so that merging is more flexible.
#Remember to eventually expand by day. 


interGpsGoogWeather<-weather_short%>%
  filter(Country %in% GpsGoog$country)%>%
  group_by(Country)

# there are no cities yet


#n_groups(interGpsGoogWeather)
#n_groups(GpsGoog)


GpsGoogWeather<-
  fuzzy_inner_join(interGpsGoogWeather,GpsGoog, 
                   by=c("Country"="country","LATITUDE"="SouthHalf", "LATITUDE"="NorthHalf","LONGITUDE"="WestHalf","LONGITUDE"="EastHalf"),
                   match_fun=list(`==`,`>=`, `<=`,`>=`,`<=`)) 

#n_groups(GpsGoogWeather)#We miss one country
#xy<-setdiff(interGoogle_GPS$Country,GpsGoogWeather$country)
#Rwanda. I will add it manually. 

RwandaGM<-GpsGoog%>%filter(country=="Rwanda")
RwandaW<-interGpsGoogWeather%>%filter(Country=="Rwanda")

Rwanda<-fuzzy_inner_join(RwandaW,RwandaGM, 
                         by=c("LATITUDE"="South", "LATITUDE"="North","LONGITUDE"="West","LONGITUDE"="East"),
                         match_fun=list(`>=`, `<=`,`>=`,`<=`)) 

GpsGoogWeather<-rbind(GpsGoogWeather,Rwanda)

match<-ifelse(GpsGoogWeather$country==GpsGoogWeather$Country,1,0)

gwm<-GpsGoogWeather%>%
  dplyr::select(LATITUDE:AvgTornado,City.x:Location,address:WestHalf)%>%
  rename(City=City.x)

US4<-gwm%>%
  group_by(Country,City)%>%
  summarise(n=n())%>%
  filter(Country=="United States")



# gwmp<-left_join(GpsGoogWeather,CityPop,by="address")
# qwmp_address<-gwmp%>%
#   filter(!is.na(population))
# summary(gwmp$population)
##Only a handful of cities are matched by name


##Goal 3: 
###Adding population information - turning gwm (gps-weather-mobility to gwmp: gps - mobility - weather - population)
CityPop<-CityPop%>%
  dplyr::select(latitude:Country)

gwmp<-
  fuzzy_left_join(gwm,CityPop, 
                   by=c("Country"="Country","SouthHalf"="latitude","NorthHalf"= "latitude","WestHalf"="longitude","EastHalf"="longitude"),
                   match_fun=list(`==`,`<=`, `>=`,`<=`,`>=`)) 

summary(gwmp$population)

xx<-setdiff(gwmp$Country.x,gwmp$Country.y)
match<-ifelse(gwmp$Country.x==gwmp$Country.y,1,0)
#as we would expect as now the merge took place conditional on the country matching. Still, we might have mismatches.


n_groups(gwmp) #No loss of countries. Good. 


US5<-gwmp%>%
  group_by(Country.x,City)%>%
  summarise(n=n())%>%
  filter(Country.x=="United States")

gwmp<-gwmp%>%
  rename(Country=Country.x)%>%
  dplyr::select(-Country.y)


#Goal 4: we need to summarise by city

#take the medians

#maybe better to extend first? Yes


#Goal 5: expand on daily basis for weather

#First, let's reduce weather to the countries we care about.
#even better, let's keep only the stations that made it in gwmp

interWeather<-weather%>%
  filter(STATION %in% gwmp$Station)%>%
  group_by(Country)%>%
  mutate(Station=STATION)

n_groups(interWeather)

#Good, now let's match gwmp by the station, unique, identifiers

gwmp_longW<-left_join(interWeather,gwmp,by=c("Station","Country"))


#some stations appear more than once
# DoubleStations<-gwmp_longW%>%
#   group_by(Country,City,Station)%>%
#   summarise(Name=first(NAME),City=first(City))%>%
#   ungroup()%>%
#   group_by(Station)%>%
#   mutate(n=n())%>%
#   filter(n>1)
 #It looks like the stations that correspond to more than one cities are close to them. 

#Of course, there will be cities with multiple stations attached to them. 
#We decided to take the median weather conditions across these stations for each day. 
#This is convenient as it solves the FHRSTT, variables that either 1 or 0. 

gwmp_longW_city<-gwmp_longW%>%
  group_by(Country,City,DATE)%>%
  summarise_each(funs(if(is.numeric(.)) median(., na.rm = TRUE) else first(.)))
#This took forever and the function is depricated. Let's try with across:

# longW_city<-gwmp_longW %>%
#   group_by(Country,City,DATE)%>%
#   summarise(
#     across(where(is.numeric), median), 
#     across(where(is.factor), first),
#     n = n(), 
#   )
#Also takes forever... Also, if not a factor, ignored...3 variables in this case
#In any case, the results are satisfying:

CityPool<-gwmp_longW_city%>%
  group_by(Country,City)%>%
  summarise_each(funs(if(is.numeric(.)) median(., na.rm = TRUE) else first(.)))%>%
  ungroup()%>%
  group_by(Country)

#n_groups(CityPool) #Good

#This should tell us how many cities we have in our subject pool. We ahave: 466 cities from 53 countries. 
#We have population information for 211 of those cities.
#


#There is a problem with the US... maybe with other countries as well. British Columbia is coded as American. It shouldn't. It is Canadian.
#This is probably a result of allowing stations to be matched with multiple cities...
#I tried to fix the problem by doing the match according to Country and Station but the problem persists...


# US6<-CityPool%>%
#   filter(Country=="United States")
# 
# CA<-CityPool%>%
#   filter(Country=="Canada")

#Goal 5: expand on daily basis for mobility
#Match 

gwmp_longW_city$City_GPS<-gwmp_longW_city$City
gwmp_longW_city$City<-gwmp_longW_city$City.y



gwmp_longWMG<-left_join(mobility_regional,gwmp_longW_city,by=c("Country","City","Date"))


gwmp<-gwmp_longWMG

#To do - check that gwmp_longWMG works. Best way to do it is through a report.
#Automate the procedure for Google? Maybe not too urgent if we focus on first wave. 

#use this command to discard everything but the final merged df
rm(list=setdiff(ls(), "gwmp"))

gwmp<-gwmp%>%
  dplyr::select(Country:Movement,lon,lat,address.x,north:South_half,STATION:FRSHTT,Temp_C,Dewp_C,Fog:Tornado,wgt:address.y,population:City_GPS)


#FRSHTT is corrupt, along wth the variables derived from it. Leading zeros are dropped somewhere (Excel?); can be fixed if needed
source("OxfordTracking/covid-policy-tracker-master/data/import_OxCGRT.R")


#gwmp2<-left_join(gwmp,Ox,by=c("Country","Date"))

#write_rds(gwmp2,"gwmp.rds")


