social_prefs<-read.csv("Briq/socialprefs.csv")%>%
  rename(Country=country)%>%
  mutate(Country=countrycode(Country,"country.name","country.name"))
