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






# g1<-GlobalBeh%>%
#   filter(Country=="United States")%>%
#   mutate(Ap=forcats::fct_relevel(react_pub_appr,
#               "The reaction is not at all sufficient",
#               "The reaction is somewhat insufficient",
#               "The reaction is appropriate",
#               "The reaction is somewhat too extreme",
#               "The reaction is much too extreme"))%>%
#   ggplot(aes(Ap))+
#   geom_bar()+
#   scale_colour_brewer(palette = 'Set1')+
#   theme_classic()+
#   labs(x="",title="Do you think the reaction of your country's public is appropriate,
#        too extreme, or not sufficient?")
#   #ggplot(aes(x=))
# 
# ggsave("Pub.png",width=12,height=9)
# 
# g2<-GlobalBeh%>%
#   filter(Country=="United States")%>%
#   mutate(Ef=forcats::fct_relevel(perceivedeffectivnes,
#                                  "Not at all effective",
#                                  "Not effective",
#                                  "Neither effective nor ineffective",
#                                  "Effective",
#                                  "Very effective"))%>%
#   ggplot(aes(Ef))+
#   geom_bar()+
#   scale_colour_brewer(palette = 'Set1')+
#   theme_classic()+
#   labs(x="",title="How effective are social distancing
# measures (e.g., through a general curfew) to slow down the spread of the coronavirus?")
# 
# 
# 
# 
# g3<-GlobalBeh%>%
#   filter(Country=="United States",govtrust!="6")%>%
#   mutate(Tr=forcats::fct_relevel(govtrust,
#                                  "Strongly distrust",
#                                  "Somewhat distrust",
#                                  "Neither trust nor distrust",
#                                  "Somewhat trust",
#                                  "Strongly trust"))%>%
#   ggplot(aes(Tr))+
#   geom_bar()+
#   scale_colour_brewer(palette = 'Set1')+
#   theme_classic()+
#   labs(x="",title="How much do you trust your country's government to take care of its citizens?")
# 
# cowplot::plot_grid(g1,g2,g3,nrow =3)
# ggsave("Trust.png",width=12,height=9)
