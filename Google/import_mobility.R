# mobility_long: daily data
# mobility_short: mobility on latest available date
library(countrycode)

mobility <- read.csv("Google/Global_Mobility_Report.csv")%>%
  filter(sub_region_1=="")%>%
  mutate(Movement=rowMeans(.[,c("retail_and_recreation_percent_change_from_baseline",
                                "grocery_and_pharmacy_percent_change_from_baseline",
                                "parks_percent_change_from_baseline", 
                                "transit_stations_percent_change_from_baseline",
                                "workplaces_percent_change_from_baseline")],na.rm=T))%>%
  mutate(Date=as.Date(date))%>%
  mutate(Country=countrycode(country_region_code,'iso2c','country.name')) %>%
  mutate(Country=replace(Country, is.na(country_region_code), 'Namibia'))



mobility_regional <- read.csv("Google/Global_Mobility_Report.csv")%>%
 # filter(sub_region_1=="")%>%
  mutate(Movement=rowMeans(.[,c("retail_and_recreation_percent_change_from_baseline",
                                "grocery_and_pharmacy_percent_change_from_baseline",
                                "parks_percent_change_from_baseline", 
                                "transit_stations_percent_change_from_baseline",
                                "workplaces_percent_change_from_baseline")],na.rm=T))%>%
  mutate(Date=as.Date(date))%>%
  mutate(Country=countrycode(country_region_code,'iso2c','country.name')) %>%
  mutate(Country=replace(Country, is.na(country_region_code), 'Namibia'))



# mobility_short<-mobility_long%>%
#   filter(Date==last(Date))
