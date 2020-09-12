#checking if there is an intuitive response to weather from Google


gwmp<-readRDS("GWMP2.rds")


modelsTemp<-gwmp%>%
  filter(!is.na(Temp_C),!is.na(Movement))%>%
  group_by(Country,City)%>%
  group_modify(~ broom::tidy(lm(Movement ~ Temp_C, data = .x)))


modelsPRCP<-gwmp%>%
  filter(!is.na(PRCP),!is.na(Movement))%>%
  group_by(Country,City)%>%
  group_modify(~ broom::tidy(lm(Movement ~ PRCP, data = .x)))



m1<-modelsTemp%>%filter(term=="(Intercept)")%>%dplyr::select(estimate)%>%rename(Intercept_Temp=estimate)
m2<-modelsTemp%>%filter(term=="Temp_C")%>%dplyr::select(estimate,p.value)%>%rename(Temp_C_coef=estimate)
m3<-modelsPRCP%>%filter(term=="(Intercept)")%>%dplyr::select(estimate)%>%rename(Intercept_PRCP=estimate)
m4<-modelsPRCP%>%filter(term=="PRCP")%>%dplyr::select(estimate,p.value)%>%rename(PRCP_coef=estimate)

m34<-left_join(m3,m4,by=c("Country","City"))

m12<-left_join(m1,m2,by=c("Country","City"))

models<-left_join(m12,m34,by=c("Country","City"))

models[,-c(1:2)]<-round(models[,-c(1:2)],3)



q<-DT::datatable(models)


###Doing the same only for residential

modelsTemp<-gwmp%>%
  filter(!is.na(Temp_C),!is.na(residential_percent_change_from_baseline))%>%
  group_by(Country,City)%>%
  group_modify(~ broom::tidy(lm(residential_percent_change_from_baseline ~ Temp_C, data = .x)))


modelsPRCP<-gwmp%>%
  filter(!is.na(PRCP),!is.na(residential_percent_change_from_baseline))%>%
  group_by(Country,City)%>%
  group_modify(~ broom::tidy(lm(residential_percent_change_from_baseline ~ PRCP, data = .x)))



m1<-modelsTemp%>%filter(term=="(Intercept)")%>%dplyr::select(estimate)%>%rename(Intercept_Temp=estimate)
m2<-modelsTemp%>%filter(term=="Temp_C")%>%dplyr::select(estimate,p.value)%>%rename(Temp_C_coef=estimate)
m3<-modelsPRCP%>%filter(term=="(Intercept)")%>%dplyr::select(estimate)%>%rename(Intercept_PRCP=estimate)
m4<-modelsPRCP%>%filter(term=="PRCP")%>%dplyr::select(estimate,p.value)%>%rename(PRCP_coef=estimate)

m34<-left_join(m3,m4,by=c("Country","City"))

m12<-left_join(m1,m2,by=c("Country","City"))

models<-left_join(m12,m34,by=c("Country","City"))

models[,-c(1:2)]<-round(models[,-c(1:2)],3)


q2<-DT::datatable(models)


summary(q$x$data$Temp_C_coef)
summary(q$x$data$PRCP_coef)


summary(q2$x$data$Temp_C_coef)
summary(q2$x$data$PRCP_coef)

##Doesn't improve the intuitiveness of the result massively. If anything the "Movement" variable made more sense. 



###########################Let's calculate regression coefs for immediate response#####################

gwmpo<-left_join(gwmp,Ox,by=c("Country","Date"))

test<-gwmpo%>%
  dplyr::select(Country,City,Date,StringencyIndex)


modelsRegCoef<-gwmpo%>%
  filter(!is.na(StringencyIndex),!is.na(Movement))%>%
  group_by(Country,City)%>%
  group_modify(~ broom::tidy(lm(Movement ~ StringencyIndex, data = .x)))


m1<-modelsRegCoef%>%filter(term=="(Intercept)")%>%dplyr::select(estimate)%>%rename(Intercept=estimate)
m2<-modelsRegCoef%>%filter(term=="StringencyIndex")%>%dplyr::select(estimate,p.value)%>%rename(SI_est=estimate)



m12<-left_join(m1,m2,by=c("Country","City"))

gwmpo2<-left_join(gwmpo,m12,by=c("Country","City"))


###

#write_rds(gwmpo2,"GWMPO.rds")


