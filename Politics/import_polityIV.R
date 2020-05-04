polityIV <- read.csv("Politics/polity4_v2018.csv")%>%
  filter(year == 2018) %>%
  mutate(Country=countrycode(scode,'p4c','country.name')) %>%
  dplyr::select('Country','polity2')
