setwd("C:/Users/fores/OneDrive - The University of Nottingham/GitHub/COVID-19")

library(ggpubr)
library(ggpmisc)

w<-read.csv("Weather_new/Athens.csv")%>%
  mutate(Date=as.Date(DATE))
m<-read.csv("Google/Global_Mobility_Report.csv")%>%
  filter(country_region=="Greece", sub_region_1=="Decentralized Administration of Attica")%>%
  mutate(Date=as.Date(date))%>%
  mutate(Movement=rowMeans(.[,c("retail_and_recreation_percent_change_from_baseline",
                                "grocery_and_pharmacy_percent_change_from_baseline",
                                "parks_percent_change_from_baseline", 
                                "transit_stations_percent_change_from_baseline",
                                "workplaces_percent_change_from_baseline")],na.rm=T))%>%
  mutate(Date=as.Date(date))


source("OxfordTracking/covid-policy-tracker-master/data/import_OxCGRT.R")

Ox<-Ox%>%
  filter(Country=="Greece")


df<-merge(merge(w,m,by="Date"),Ox,by="Date")

df$Movement_1<-dplyr::lag(df$Movement)

cor.test(df$TEMP,df$parks_percent_change_from_baseline)
cor.test(df$TEMP,df$retail_and_recreation_percent_change_from_baseline)

cor.test(df$TEMP,df$Movement)
cor.test(df$PRCP,df$Movement)

cor.test(df$StringencyIndex,df$Movement)

my.formula<-y~x


df%>%
  ggplot(aes(x=TEMP,y=Movement))+
  geom_point(size=4,alpha=0.5)+
  stat_smooth(method="lm")+
  stat_cor(method = "pearson",label.y=c(-1.25), size=4)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  theme_bw()+
  theme(text = element_text(size=15))
  



df%>%
  ggplot(aes(x=PRCP,y=Movement))+
  geom_point(size=4,alpha=0.5)+
  stat_smooth(method="lm")+
  stat_cor(method = "pearson",label.y=c(-1.25), size=4)+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  theme_bw()+
  theme(text = element_text(size=15))


base<-formula(paste("Movement ~", paste(c("StringencyIndex"), sep = "", collapse = "+")))
base_mob<-lm(data=df,base)

temp<-formula(paste("Movement ~", paste(c("StringencyIndex","TEMP"), sep = "", collapse = "+")))
temp_mob<-lm(data=df,temp)

weather<-formula(paste("Movement ~", paste(c("StringencyIndex","TEMP","PRCP"), sep = "", collapse = "+")))
weather_mob<-lm(data=df,weather)


tab_model(base_mob,temp_mob,weather_mob)



  