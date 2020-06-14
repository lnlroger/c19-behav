df<-readRDS("df_covid_long.rds")%>%
  filter(sub_region_1=="")  ###taking the country level average 


basic<-c("Date","week","Country","Continent")


covid<-c("total_cases", "new_cases", "total_deaths", "new_deaths")

economy<-c("Gini","GDP.capita","polity2")

behavioural<-c("COL","ROL","MAS","UAI","LTO","IND","patience","risktaking","altruism","trust")

GoogleVars<-c('retail_and_recreation_percent_change_from_baseline', 'grocery_and_pharmacy_percent_change_from_baseline', 'parks_percent_change_from_baseline', 'transit_stations_percent_change_from_baseline', 'workplaces_percent_change_from_baseline','residential_percent_change_from_baseline','Movement')

Policy<-c("StringencyIndex")

