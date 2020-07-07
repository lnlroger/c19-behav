hf<-read.csv("Collectivism/hofstede.csv")%>%
  mutate(country=countrycode(country,"country.name","country.name"))%>%
  rename(Country=country)%>%
  mutate_at(vars(-Country, -ctr), as.numeric)%>%
  mutate(COL=100-IDV)
