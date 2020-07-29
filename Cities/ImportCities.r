#city_big<-read.csv("Cities/worldcitiespop.csv")
##The file is huge so I focus only on the top 10 most populated cities per country.
##I group by Country and Region, not city, because the city level is too granular. For example, it includes all counties of Athens.
# city<-city_big%>%
#   filter(!is.na(Population))%>%
#   mutate(Country=as.character(Country))%>%
#   group_by(Country,Region)%>%
#   arrange(Population)%>%
#   summarise(latitude=mean(Latitude),longitude=mean(Longitude),population=sum(Population),city=last(City))%>%
#   top_n(10,population)%>%
#   rename(CountryCode=Country)%>%
#   mutate(Country=countrycode(CountryCode,origin='iso2c',destination='country.name'))
# 
# setwd("C:/Users/fores/OneDrive - The University of Nottingham/GitHub/COVID-19/Cities")
# write.csv(city,"major_cities.csv")
# setwd("C:/Users/fores/OneDrive - The University of Nottingham/GitHub/COVID-19")

#city<-read.csv("Cities/major_cities.csv")
#city$Location<-apply(city[,c("city","Country")],1,paste,collapse=", ")

#locCity<- mutate_geocode(city,Location,output="more")

#write_rds(locCity,"CityPop.rds")  

CityPop<-readRDS("CityPop.rds")
