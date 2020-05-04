rol<-read.csv("WB/RuleOfLaw2018.csv")%>%
  mutate(Country=Country.Name,ROL=as.numeric(levels(X2018..YR2018.))[X2018..YR2018.])%>%
  mutate(Country=countrycode(Country,'country.name','country.name')) %>%
  dplyr::select(Country,Country.Code,ROL)%>%
  mutate(Country.Code=recode_factor(Country.Code,
                                    'ROM'="ROU"))

#flu<-read.csv("influenza-vaccination-rates.csv")%>%
#group_by(Country.Code)%>%
#summarise(Time=last(time),VaccinationRate=last(value))%>%
#mutate(OECE=1)


#RolFlu<-merge(rol,flu,by="Country.Code",all=T)

#countries<-read.csv(here::here("Countries.csv"))%>%
#mutate(Country=trimws(Country))%>%
# mutate(Population:Service=as.numberic(Population:Service))

#WORLD BANK
# wb<-merge(merge(merge(merge(
#                 read.csv("WB_GDP_pc.csv"),
#                 read.csv("WB_unemployment.csv"),by="Country.Code",all=T),
#                 read.csv("WB_communicable.csv"),by="Country.Code",all=T),
#                 read.csv("WB_1564.csv"),by="Country.Code",all=T),
#                 read.csv("WB_65up.csv"),by="Country.Code",all=T)

#write.csv(wb,"WorldBank.csv")


wb<-read.csv("WB/WorldBank.csv")%>%
  mutate(Country=countrycode(Country.Code,'wb','country.name'))%>%
  mutate(log_GDP_pc=log(GDP_pc))


wb<-merge(wb,read.csv("WB/WB_hosp_bed.csv"),by="Country.Code",all=T) 