Gf_gov<-read.csv("Government_Gelfand/mortality_likelihood_dat.csv")%>%
  mutate(Country=Entity2)%>%
  mutate(Country = recode(Country, "Timor" ="Timor Leste", "Saint Barthlemy" = "Saint Barthelemy", "Guya" = "Guyana")) %>%
  filter(Country %notin% c("Boire Sint Eustatius and Saba", "Greda", "Northern Maria Islands", "Tunesia")) %>%
  mutate(Country=countrycode(Country,"country.name","country.name"))
