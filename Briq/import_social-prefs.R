social_prefs<-read.csv("Briq/Ind_level_v11.csv")%>%
  rename(Country=country)%>%
  rename(City=region)%>%
  mutate(Country=countrycode(Country,"country.name","country.name"))%>%
  group_by(Country)%>%
  summarise_at(vars(wgt:subj_math_skills),mean,na.rm=TRUE)

#Weirdly, when I aggregate from the individual level to the country level, the results are slightly different than 
#their version of summarised data... I tried taking median instead of mean and the results are even more different...


# social_prefs_city<-read.csv("Briq/Ind_level_v11.csv")%>%
#   rename(Country=country)%>%
#   rename(City=region)%>%
#   mutate(Country=countrycode(Country,"country.name","country.name"))%>%
#   group_by(Country,City)%>%
#   summarise_at(vars(wgt:subj_math_skills),mean,na.rm=TRUE)
