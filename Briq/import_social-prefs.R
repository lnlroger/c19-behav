#social_prefs<-read.csv("Briq/Ind_level_v11.csv")%>%
#  rename(Country=country)%>%
#  rename(City=region)%>%
#  mutate(Country=countrycode(Country,"country.name","country.name"))%>%
#  group_by(Country)%>%
#  summarise_at(vars(wgt:subj_math_skills),mean,na.rm=TRUE)

#Weirdly, when I aggregate from the individual level to the country level, the results are slightly different than 
#their version of summarised data... I tried taking median instead of mean and the results are even more different...


# social_prefs_city<-read.csv("Briq/Ind_level_v11.csv")%>%
#   rename(Country=country)%>%
#   rename(City=region)%>%
#   mutate(Country=countrycode(Country,"country.name","country.name"))%>%
#   group_by(Country,City)%>%
#   summarise_at(vars(wgt:subj_math_skills),mean,na.rm=TRUE)
# 
# 
# social_prefs_city$City<-gsub("\\,.*","",social_prefs_city$City) #There were ", excl Toronto" that are misinterpreted by Google - removed them
# social_prefs_city$Location<-apply(social_prefs_city[,c("City","Country")],1,paste,collapse=", ")

#This code creates a geolocation. I create it once and save it as .csv to not abuse my API.
#The option "more" is crucial. It generates ranges for coordinates and returns the name that Google thought you meant

#loc<-mutate_geocode(social_prefs_city,Location,output="more")  
#write.csv(loc,"SocialPrefsGeo.csv")
gps_coord<-read.csv("Briq/SocialPrefsGeo.csv")%>%
   filter(City!="")






