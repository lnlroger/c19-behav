#Source: https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-day?pageSize=100&dataTypes=%5B%22DEWP%22,%22FRSHTT%22,%22GUST%22,%22MAX%22,%22MIN%22,%22MXSPD%22,%22PRCP%22,%22SLP%22,%22SNDP%22,%22STP%22,%22TEMP%22,%22VISIB%22,%22WDSP%22%5D&bbox=%5B%2239.266,22.500,36.880,25.357%22%5D&startDate=%5B%222019-12-01T00:00:00%22%5D&endDate=%5B%222020-01-01T23:59:59%22%5D
library("lubridate")
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
source("ImportLong.r")




#Goal 1: merge google_cities with gps_cities while minimising data loss
#we now have east and west (lon) and north and south (lat). These are ranges for longitude and latitude. 
#Test: the merged file should have 53 countries. This is the intersection of Google_city and GPS at the country level. 
#To make matching easier, I start by creating a subset of google's mobility_regional that uses only the intersection with GPS. 

interGoogle_GPS<-short_mob%>%
  filter(Country %in% gps_coord$Country)%>%
  group_by(Country)%>%
  mutate(number=n())

n_groups(interGoogle_GPS) #Good, back to 53, though, we prob. don't have city-level info for Croatia's GPS.

interGPS_Google<-gps_coord%>%
  filter(Country %in% mobility_regional$Country)%>%
  group_by(Country)%>%
  mutate(number=n())

n_groups(interGPS_Google)


#South is the city's min lat, North is the city's max lat. West is min long and East max long. 
#We are doing a fuzzy_inner_join taking the lat from Google and placing it between the min and max from gps_coord & same for longitude
#The ranges are pretty big for some cities, so we might end up with multiples... Not sure how these will be treated.

GpsGoo<-
  fuzzy_inner_join(interGPS_Google, interGoogle_GPS, 
                   by=c("lat"="SouthHalf", "lat"="NorthHalf","lon"="WestHalf","lon"="EastHalf"),
                   match_fun=list(`>=`, `<=`,`>=`,`<=`)) 

#Gets rid of double entries
GpsGoog<-GpsGoo%>%
  group_by(lat,lon)%>%
  summarise_all(first)%>%
  ungroup()%>%
  group_by(Country.x)


n_groups(GpsGoog)


# Problem: locations like "East Middlands" from GPS, do not have coordinates because Google does not recognise them. 
# Fixed it manually in import_mobility.r
# Let's list all those with NA in lon or lat

missing<-interGPS_Google%>%
  filter(is.na(lat))

#Nothing is missing anymore..Good!







#Goal 2: add weather for every city

#We will use the short_weather so that merging is more flexible.
#Remember to eventually expand by day. 


GpsGoogWeather<-
  fuzzy_inner_join(weather_short,GpsGoog, 
                   by=c("LATITUDE"="SouthHalf", "LATITUDE"="NorthHalf","LONGITUDE"="WestHalf","LONGITUDE"="EastHalf"),
                   match_fun=list(`>=`, `<=`,`>=`,`<=`)) 

temCity<-GpsGoogWeather%>%
  group_by(Country.x,City.x)%>%
  summarise_all(first)

#Why do temCity and tempCoord have different lengths?

tempCoord<-GpsGoogWeather%>%
  group_by(LATITUDE,LONGITUDE)%>%
  summarise_all(first)

n_groups(GpsGoogWeather)
#52 countries instead of 53.. oh well!



#Gets rid of double entries
GpsGoog<-GpsGoo%>%
  group_by(lat,lon)%>%
  summarise_all(first)%>%
  ungroup()%>%
  group_by(Country.x)





#Add population info

#Adding info on population. This is found in "city" data frame. 
#I plan to do a fuzzy_join by South_half, etc. 














#Take average temperature for a city
d_red<-d%>%
  group_by(Country,City)%>%
  summarise(celcius=mean(Celcius),sd_celcius=sd(Celcius),prcp=mean(PRCP),sd_prcp=sd(PRCP))




#city<-read.csv("Cities/worldcitiespop.csv")



#social_prefs_city$Location<-apply(social_prefs_city[,c("City","Country")],1,paste,collapse=", ")
#coord<-geocode(social_prefs_city$Location)

#briq_coord<-social_prefs_city
#briq_coord$Location<-apply(social_prefs_city[,c("City","Country")],1,paste,collapse=", ")
#briq_coord$Lat<-coord$lat
#briq_coord$Lon<-coord$lon

#write.csv(briq_coord,"City_level_v11_coord.csv")

#merge with Briq
#First, let's keep only the relevant countries





#Lat is in left df, lat_min is in right. We want Lat to be '>=' than Lat_min...etc 
GPS<-
  fuzzy_left_join(GPS_coord, city2, 
                by=c("Lat"="Lat_min_5", "Lat"="Lat_max_5","Lon"="Lon_min_5","Lon"="Lon_max_5"),
                match_fun=list(`>=`, `<=`,`>=`,`<=`))

# 

#Merging with Google Movement 
## When Movement is the left df, merging takes forever...and concludes "Error: cannot allocate vector of size 240.1 Mb" 
#the right one takes long as well: 15 mins approx.
## Merge Google with Briq, right_join, with +-0.2 geocides

c<-mobility_regional%>%
  filter(Country %in% briq_coord$Country)
#reducing the data set so that the fuzzy join concludes faster.. But it reduces the number of observations to 53...
#This is because there are 18 countries that we lose because of no Movement info at the city level. 

BriqGoo<-
  fuzzy_inner_join(c, briq_coord_pop, 
                   by=c("lat"="Lat_min", "lat"="Lat_max","lon"="Lon_min","lon"="Lon_max"),
                   match_fun=list(`>=`, `<=`,`>=`,`<=`)) 

q<-BriqGoo%>%
  group_by(Country)%>%
  summarise(n()/130)

q$Country

US<-BriqGoo%>%
  filter(Country=="United States")

# It doesn't work with 0.2 very well. We get only 42 countries from a possible of 71, which is the initial intersection between Google and Briq 
# The cities are also weird. From the US we only get 2: Florida and Massachusetts while from.. Nicaragua 9
# I will increase the range of Lat and Lon from 0.2 to 0.4 (Lat_min_4) for this and reduce it again for the weather. 

briq_coord_pop<-briq_coord_pop%>%
  mutate(Lat_min_4=Lat-0.2,
         Lat_max_4=Lat+0.2,
         Lon_min_4=Lon-0.2,
         Lon_max_4=Lon+0.2)


BriqGoo<-
  fuzzy_inner_join(c, briq_coord_pop, 
                   by=c("lat"="Lat_min_4", "lat"="Lat_max_4","lon"="Lon_min_4","lon"="Lon_max_4"),
                   match_fun=list(`>=`, `<=`,`>=`,`<=`)) 

q<-BriqGoo%>%
  group_by(Country)%>%
  summarise(n()/130)

q$Country



qc<-c%>%
  group_by(Country)%>%
  summarise(n()/130)


# Nope, didn't help the way I had hoped. Still only 42 countries. Still only 2 from the US, but now Nicaragua has 42...!

##Challenge 1: Which 11 countries do we lose because of bad match? 
##Challenge 2: How do we include cities from US, Canada and Australia? These are countries that are big, so range might need adjustment.

##Challenge 1: do an antijoin between BriqGoo and c

antiC<-c%>%
  anti_join(BriqGoo,by="Country")%>%
  group_by(Country)%>%
  summarise(n())

antiC$Country

## OK, why do we lose these countries? 
## Because the coordinates from Google_cities did not match with any of the coordinates of the cities in Briq. 
## How far were they?

c1<-c%>%
  filter(Country %in% antiC$Country)%>%
  group_by(Country,City)%>%
  summarise(lt=mean(lat),lon=mean(lon))

b1<-briq_coord_pop%>%
  filter(Country.x %in% c1$Country)%>%
 # group_by(Country.x,City)%>%
  group_by(Country.x,City)%>%
  summarise(lt=mean(Lat),lon=mean(Lon),loc=first(Location))

#It appears that many cities in Briq_coord_pop do not have geocedes. Maybe due to the mergure with cities
b2<-briq_coord%>%
   filter(Country %in% c1$Country)%>%
  # group_by(Country.x,City)%>%
  group_by(Country,City)%>%
  summarise(lt=mean(Lat),lon=mean(Lon),loc=first(Location))


#Yup, so I need to go back and fix the mergure with population and keep the coordinates of Briq NOT of the city. 
#Actually, it's already there. I just need to refer to Lat instead of latitude

# Challenge 2: I will see how far the state's 

revgeocode(c(lon=-79.4,lat=43.7))

# I get why Canada and US get screwed up, but I don't get why Bolivia, Beni, for example, doesn't get matched. Both lon and lat are within Lon_4 and Lat_4
