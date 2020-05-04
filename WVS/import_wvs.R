wvs<-read.csv("WVS/WVS_per_Country.csv")%>%
  #filter(Wave==6)%>%
  dplyr::select(Country,E235,E236,E124,E229,Wave)%>%
  mutate(Country=countrycode(Country,'country.name','country.name')) %>%
  rename(Democracy=E235,Democraticness=E236,Civil=E124,Opression=E229)%>%
  group_by(Country)%>%
  filter(Wave==max(Wave)) #I keep the values for each country with the most recent available wave