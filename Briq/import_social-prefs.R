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
  group_by(Country)


# missing<-gps_coord%>%
#   filter(is.na(lat))%>%
#   mutate(Location=dplyr::recode_factor(Location,"Chugoku, Japan"="Shimonoseki, Japan",
#                          "Costa Centro (Central Coast), Peru" ="Costa Centro, Peru",
#                          "Costa Norte (North Coast), Peru" = "Costa Norte, Peru",
#                          "Lima Metropolitana (Lima Metropolis), Peru" = "Lima Metropolitana, Peru",
#                          "Sierra Norte (North Sierra/ North Mountain), Peru"="Sierra Norte, Peru",
#                          "Mellersta Norrland, Sweden" = "Sundsvall, Sweden",
#                          "Norra Mellansverige, Sweden" = "Gavle, Sweden",
#                          "Ostra Mellansverige, Sweden" = "Uppsala, Sweden",
#                          "Vastverige, Sweden"="Gothenburg, Sweden",
#                          "Espace Mittelland, Switzerland" = "Bern, Switzerland",
#                          "East Midlands, United Kingdom" = "Nottingham, United Kingdom",
#                          "East of England, United Kingdom" = "Norwich, United Kingdom",
#                          "North East, United Kingdom" = "Newcastle, United Kingdom",
#                          "North West, United Kingdom"= "Manchester, United Kingdom",
#                          "Zone II: North-Western, Venezuela"="North-Western, Venezuela",
#                          "Zone III: Eastern, Venezuela" = "Anzoategui, Monagas, Sucre, Venezuela",
#                          "Zone IV: Centre Llano, Venezuela" = "Centre Llano, Venezuela ",
#                          "Zone VI: Andean, Venezuela" = "Andean, Venezuela"  
#                          ))%>%
#   mutate(Location=as.character(Location))
#   
# 
# missing_loc<-missing%>%
#   dplyr::select(X:Location)
#   
# missing_loc<-mutate_geocode(missing_loc, Location, output="more")
# 
# #write.csv(missing_loc,"missing.csv")
# 
# gps_coord<-gps_coord%>%
#   filter(!is.na(lat))
# 
# gps_coord<-rbind(gps_coord,missing_loc)
# 
# gps_coord<-gps_coord%>%
#   group_by(Country)
# 
# write.csv(gps_coord,"Briq/SocialPrefsGeo.csv")
