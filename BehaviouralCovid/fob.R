#q<-read.csv("C:/Users/fores/Desktop/COVID19_personal/COVID19-BehPerceptions/GlobalBehaviorsPerceptions_Dataset_May01_2020.csv")
#saveRDS(q,"GlobalBehCovid.rds")

#see here: https://www.nber.org/papers/w27082 for paper
#and here for data: (https://osf.io/3sn2k/)
#I believe the data collection period was between 20/03 and 08/04. Or at least the one that they made available online and that I am currently working with.
#They append data on government policy from Oxford:  https://www.bsg.ox.ac.uk/research/publications/variation-government-responses-covid-19 


library(lubridate)

GlobalBeh<-readRDS("BehaviouralCovid/GlobalBehCovid.rds")%>%
  rename(Country=CountryofLiving)%>%
  mutate(Country=countrycode::countrycode(Country,"country.name","country.name"))%>%
  mutate(Date=lubridate::dmy(StartDate))%>%
  mutate(week=lubridate::floor_date(Date,"week"))

  #mutate_at(vars(beh_stayhome:weight_sample),.funs=as.numeric)






g<-GlobalBeh%>%
  group_by(Country)%>%
  mutate(total_obs=n())%>%
  #filter(total_obs>500)%>%
  filter(total_obs>500)%>%
  mutate_at(vars(fob_social:fob_curfew),.funs=as.numeric)%>%
  group_by(country,week)%>%
  summarise(Gatherings=mean(fob_social-1,na.rm=T),n=n(),Total=first(total_obs),se=sd(fob_social-1)/sqrt(n-1))%>%
  filter(n>25)%>%
  ggplot(aes(x=week,y=Gatherings))+
  geom_point()+
  #stat_smooth()+
  
  geom_line(color="blue")+
  geom_errorbar(aes(ymin = Gatherings-se,ymax = Gatherings+se),
                width = 0.4,
                position = position_dodge(0.9))+
  facet_wrap(~country)+
  #facet_grid(Over50~Vintage)+
  # geom_text(aes(x=as.Date("2018-07-01"),y=5,label=paste0("JL:",Recom,": " ,ExpScore),col=Recom),size=2 ,vjust = "inward", hjust = "inward")+
  
  theme_bw()+
  theme(legend.position="none")

h<-GlobalBeh%>%
  group_by(Country)%>%
  mutate(total_obs=n())%>%
  #filter(total_obs>500)%>%
  filter(total_obs>500)%>%
  mutate_at(vars(fob_social:fob_curfew),.funs=as.numeric)%>%
  group_by(country,week)%>%
  summarise(Handshakes=mean(fob_handshake-1,na.rm=T),n=n(),Total=first(total_obs),se=sd(fob_handshake-1)/sqrt(n-1))%>%
  filter(n>25)%>%
  ggplot(aes(x=week,y=Handshakes))+
  geom_point()+
  #stat_smooth()+
  
  geom_line(color="blue")+
  geom_errorbar(aes(ymin = Handshakes-se,ymax = Handshakes+se),
                width = 0.4,
                position = position_dodge(0.9))+
  facet_wrap(~country)+
  #facet_grid(Over50~Vintage)+
  # geom_text(aes(x=as.Date("2018-07-01"),y=5,label=paste0("JL:",Recom,": " ,ExpScore),col=Recom),size=2 ,vjust = "inward", hjust = "inward")+
  
  theme_bw()+
  theme(legend.position="none")


st<-GlobalBeh%>%
  group_by(Country)%>%
  mutate(total_obs=n())%>%
  #filter(total_obs>500)%>%
  filter(total_obs>500)%>%
  mutate_at(vars(fob_social:fob_curfew),.funs=as.numeric)%>%
  group_by(country,week)%>%
  summarise(Stores=mean(fob_stores-1,na.rm=T),n=n(),Total=first(total_obs),se=sd(fob_stores-1)/sqrt(n-1))%>%
  filter(n>25)%>%
  ggplot(aes(x=week,y=Stores))+
  geom_point()+
  #stat_smooth()+
  
  geom_line(color="blue")+
  geom_errorbar(aes(ymin = Stores-se,ymax = Stores+se),
                width = 0.4,
                position = position_dodge(0.9))+
  facet_wrap(~country)+
  #facet_grid(Over50~Vintage)+
  # geom_text(aes(x=as.Date("2018-07-01"),y=5,label=paste0("JL:",Recom,": " ,ExpScore),col=Recom),size=2 ,vjust = "inward", hjust = "inward")+
  
  theme_bw()+
  theme(legend.position="none")


crf<-GlobalBeh%>%
  group_by(Country)%>%
  mutate(total_obs=n())%>%
  #filter(total_obs>500)%>%
  filter(total_obs>500)%>%
  mutate_at(vars(fob_social:fob_curfew),.funs=as.numeric)%>%
  group_by(country,week)%>%
  summarise(Curfews=mean(fob_curfew-1,na.rm=T),n=n(),Total=first(total_obs),se=sd(fob_curfew-1)/sqrt(n-1))%>%
  filter(n>25)%>%
  ggplot(aes(x=week,y=Curfews))+
  geom_point()+
  #stat_smooth()+
  
  geom_line(color="blue")+
  geom_errorbar(aes(ymin = Curfews-se,ymax = Curfews+se),
                width = 0.4,
                position = position_dodge(0.9))+
  facet_wrap(~country)+
  #facet_grid(Over50~Vintage)+
  # geom_text(aes(x=as.Date("2018-07-01"),y=5,label=paste0("JL:",Recom,": " ,ExpScore),col=Recom),size=2 ,vjust = "inward", hjust = "inward")+
  
  theme_bw()+
  theme(legend.position="none")

#cowplot::plot_grid(g,h,st,crf)
# ggsave("Trust.png",width=12,height=9)
