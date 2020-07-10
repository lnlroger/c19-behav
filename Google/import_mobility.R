# mobility_long: daily data
# mobility_short: mobility on latest available date

library(countrycode)

loc<-read.csv("Google/GoogleMobGeo.csv")

mobility <- read.csv("Google/Global_Mobility_Report.csv")%>%
  rename(City=sub_region_1)%>%
  filter(City=="")%>%
  mutate(Movement=rowMeans(.[,c("retail_and_recreation_percent_change_from_baseline",
                                "grocery_and_pharmacy_percent_change_from_baseline",
                                "parks_percent_change_from_baseline", 
                                "transit_stations_percent_change_from_baseline",
                                "workplaces_percent_change_from_baseline")],na.rm=T))%>%
  mutate(Date=as.Date(date))%>%
  mutate(Country=countrycode(country_region_code,'iso2c','country.name')) %>%
  mutate(Country=replace(Country, is.na(country_region_code), 'Namibia'))

cities<-read.csv("Google/Cities.csv")

mobility_regional <- read.csv("Google/Global_Mobility_Report.csv")%>%
  rename(City=sub_region_1)%>%
  mutate(City=iconv(City,from="UTF-8",to="ASCII//TRANSLIT"))%>%
  filter(!City=="")%>%
  #filter(sub_region_1%in%cities$City1|sub_region_1%in%cities$City2)%>%
  mutate(Date=as.Date(date))%>%
  mutate(Country=countrycode(country_region_code,'iso2c','country.name')) %>%
  mutate(Country=replace(Country, is.na(country_region_code), 'Namibia')) %>%
  mutate(Movement=rowMeans(.[,c("retail_and_recreation_percent_change_from_baseline",
                                "grocery_and_pharmacy_percent_change_from_baseline",
                                "parks_percent_change_from_baseline", 
                                "transit_stations_percent_change_from_baseline",
                                "workplaces_percent_change_from_baseline")],na.rm=T))%>%
  dplyr::group_by(Country,City,date)%>%
  #dplyr::summarise(Movement=mean(Movement))
  dplyr::summarise_at(vars("retail_and_recreation_percent_change_from_baseline":"Movement"),
                      mean,na.rm=T)%>%
  left_join(loc,by=c("Country","City"))%>%
  mutate(EastWest=abs(east-west),
         NorthSouth=abs(north-south),
         East_half=lon+EastWest/4,
         West_half=lon-EastWest/4,
         North_half=lat+NorthSouth/4,
         South_half=lat-NorthSouth/4)


### Get geocoding for every city

short_mob<-mobility_regional%>%
   group_by(Country,City)%>%
   summarise(Move=mean(Movement),Lat=mean(lat),Lon=mean(lon),East=mean(east),West=mean(west),North=mean(north),South=mean(south),Address=first(address),
             NorthHalf=mean(North_half),SouthHalf=mean(South_half),EastHalf=mean(East_half),WestHalf=mean(West_half))
# 
# short$Location<-apply(short[,c("City","Country")],1,paste,collapse=", ")

#Obtaining geocodes using my API. Don't do it liberally. 
#loc<-mutate_geocode(short,Location,output="more")

#write.csv(loc,"GoogleMobGeo.csv")


## Some countries do not have city-level info

   