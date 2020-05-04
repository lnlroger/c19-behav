elections<-read.csv("Politics/DPI2017_basefile_Jan2018.csv") %>%
  mutate(ifs = recode(ifs, "ROM" = "ROU", "TMP" = "TLS", "ZAR" = "COD")) %>%
  filter(ifs %notin% c("CSK", "DDR", "SUN", "YMD", "YSR", "0", "")) %>%
  mutate(Country=countrycode(ifs,'wb','country.name')) %>%
  arrange(Country,year) %>%
  group_by(Country)%>%
  summarise(ElectionYear=last(year),ElectionWin=last(percent1),Country.Code=first(ifs))%>%
  naniar::replace_with_na_at(.vars = c("ElectionYear","ElectionWin","Country.Code"),
                             condition = ~.x == -999)%>%
  drop_na()