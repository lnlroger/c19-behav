hf<-read.csv("Collectivism/hofstede.csv")%>%
  mutate(Country=countrycode(Country,"country.name","country.name"))%>%
  mutate(COL=100-IDV)
