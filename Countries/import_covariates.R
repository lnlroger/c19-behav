countries<-read.csv("Countries/countries_custom.csv")%>%
  #naniar::replace_with_na_at(.vars!=c("Population","Service"),condition = ~.x == -999.000)%>%
  na_if(.,-999)%>%
  mutate(Country = recode(Country, "Central African Rep. " = "Central African Republic", 
                          "Virgin Islands " = "U.S. Virgin Islands")) %>%
  mutate(Country=countrycode(Country,"country.name","country.name"))
