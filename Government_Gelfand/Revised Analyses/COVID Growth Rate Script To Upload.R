
#######################################################################################################
#####Cultural and Institutional Factors Associated with Early Growth Rates of the COVID-19 Pandemic####
#######################################################################################################

################################################################################################
#########################Direct questions to joshcj@live.unc.edu################################
################################################################################################

library(forecast)
library(lavaan)
library(lmtest)
library(lme4)
library(lmerTest)
library(foreign)
library(ggplot2)
library(scales)
library(lubridate)
library(rockchalk)
library(simpleboot)
source("hilow.R")
library(dplyr)
library(tidyr)

############################################################
##Estimating the nation-specific exponential growth curves##
############################################################

case<-read.csv("~/Desktop/University of Maryland/Maryland Manuscripts/Active/COVID Growth/Data and Code to Upload/Infection Rate Analysis/tests-vs-confirmed-cases-covid-19-per-million.csv")

case<-case[case$Total.confirmed.cases.of.COVID.19.per.million.people..cases.per.million.!=0,]

case$log_casespermil<-log(case$Total.confirmed.cases.of.COVID.19.per.million.people..cases.per.million.)
case$day<-ave(case$log_casespermil, case$Entity, FUN = seq_along)
case$log_day<-log(case$day)
case$Ztests_per_mil<-(0-scale(case$Total.COVID.19.tests.per.million.people))+1
case$Ztests_per_mil<-ifelse(is.na(case$Ztests_per_mil)==T,0,case$Ztests_per_mil)

##Compare model fits
summary(fit1<-lmer(log_casespermil~log_day+(log_day|Entity),REML = F,data=case))
summary(fit2<-lmer(log_casespermil~day+(day|Entity),REML = F,data=case)) ##Does not converge

##Estimate trajectory and control for testing prevalence where it is available
fits <- lmList(log_casespermil ~ log_day + Ztests_per_mil | Entity, data=case)
coef<-data.frame(Country=rownames(coef(fits)),coef(fits),check.names=F)

case<-case[is.na(case$Code)==F,]
no.of.days<- case %>% group_by(Entity) %>% tally()

coef$Entity<-coef$Country
coef2<-merge(no.of.days,coef,by="Entity",all=T)

#####################################
##Factors predicting infection rate##
#####################################

d1<-read.csv("~/Desktop/University of Maryland/Maryland Manuscripts/Active/COVID Growth/Nature Human Behavior/Revision/Code and Data to Upload/country.vars.csv")
d2<-read.csv("~/Desktop/University of Maryland/Maryland Manuscripts/Active/COVID Growth/Nature Human Behavior/Revision/Code and Data to Upload/growth.coefficients.csv")

d<-merge(d1,d2,by="Entity",all=T)
d<-d[is.na(d$Tightness)==F,]

################################################
##############Data Transformations##############
################################################

##Distribution
hist(d$log_day)

#Centering variables
d$Tightness_c<-d$Tightness-mean(d$Tightness,na.rm=T)
d$Tightness_l<-d$Tightness-(mean(d$Tightness,na.rm=T)-sd(d$Tightness,na.rm=T))
d$Tightness_h<-d$Tightness-(mean(d$Tightness,na.rm=T)+sd(d$Tightness,na.rm=T))
d$Tightness_2<-d$Tightness_c*d$Tightness_c

d$CompletedTightness_c<-d$CompletedTightness-mean(d$CompletedTightness,na.rm=T)
d$CompletedTightness_l<-d$CompletedTightness-(mean(d$CompletedTightness,na.rm=T)-sd(d$CompletedTightness,na.rm=T))
d$CompletedTightness_h<-d$CompletedTightness-(mean(d$CompletedTightness,na.rm=T)+sd(d$CompletedTightness,na.rm=T))
d$CompletedTightness_2<-d$CompletedTightness_c*d$CompletedTightness_c

d$TL33_c<-d$TL33-mean(d$TL33,na.rm=T)
d$TL33_l<-d$TL33-(mean(d$TL33,na.rm=T)-sd(d$TL33,na.rm=T))
d$TL33_h<-d$TL33-(mean(d$TL33,na.rm=T)+sd(d$TL33,na.rm=T))
d$TL33_2<-d$TL33_c*d$TL33_c

d$Powerdistance_c<-d$Powerdistance-mean(d$Powerdistance,na.rm=T)
d$Powerdistance_l<-d$Powerdistance-(mean(d$Powerdistance,na.rm=T)-sd(d$Powerdistance,na.rm=T))
d$Powerdistance_h<-d$Powerdistance-(mean(d$Powerdistance,na.rm=T)+sd(d$Powerdistance,na.rm=T))
d$Powerdistance_2<-d$Powerdistance_c*d$Powerdistance_c

d$Collectivism<-(0-d$Individualism)+100
d$Collectivism_c<-d$Collectivism-mean(d$Collectivism,na.rm=T)
d$Collectivism_l<-d$Collectivism-(mean(d$Collectivism,na.rm=T)-sd(d$Collectivism,na.rm=T))
d$Collectivism_h<-d$Collectivism-(mean(d$Collectivism,na.rm=T)+sd(d$Collectivism,na.rm=T))
d$Collectivism_2<-d$Collectivism_c*d$Collectivism_c

d$Government.efficiency_c<-d$Government.efficiency-mean(d$Government.efficiency,na.rm=T)
d$Government.efficiency_l<-d$Government.efficiency-(mean(d$Government.efficiency,na.rm=T)-sd(d$Government.efficiency,na.rm=T))
d$Government.efficiency_h<-d$Government.efficiency-(mean(d$Government.efficiency,na.rm=T)+sd(d$Government.efficiency,na.rm=T))
d$Government.efficiency_2<-d$Government.efficiency_c*d$Government.efficiency_c

d$Government.effectiveness.index_c<-d$Government.effectiveness.index-mean(d$Government.effectiveness.index,na.rm=T)
d$Government.effectiveness.index_l<-d$Government.effectiveness.index-(mean(d$Government.effectiveness.index,na.rm=T)-sd(d$Government.effectiveness.index,na.rm=T))
d$Government.effectiveness.index_h<-d$Government.effectiveness.index-(mean(d$Government.effectiveness.index,na.rm=T)+sd(d$Government.effectiveness.index,na.rm=T))
d$Government.effectiveness.index_2<-d$Government.effectiveness.index_c*d$Government.effectiveness.index_c

d$WCY17GE_c<-d$WCY17GE-mean(d$WCY17GE,na.rm=T)
d$WCY17GE_l<-d$WCY17GE-(mean(d$WCY17GE,na.rm=T)-sd(d$WCY17GE,na.rm=T))
d$WCY17GE_h<-d$WCY17GE-(mean(d$WCY17GE,na.rm=T)+sd(d$WCY17GE,na.rm=T))
d$WCY17GE_2<-d$WCY17GE_c*d$WCY17GE_c

##Creaing Bureaucratic Quality Index
d$Bur_Qual<-rowMeans(data.frame(d$EIU17GE,d$PRS17GE),na.rm=T)

d$Bur_Qual_c<-d$Bur_Qual-mean(d$Bur_Qual,na.rm=T)
d$Bur_Qual_l<-d$Bur_Qual-(mean(d$Bur_Qual,na.rm=T)-sd(d$Bur_Qual,na.rm=T))
d$Bur_Qual_h<-d$Bur_Qual-(mean(d$Bur_Qual,na.rm=T)+sd(d$Bur_Qual,na.rm=T))
d$Bur_Qual_2<-d$Bur_Qual_c*d$Bur_Qual_c

##Standardizing covariates
d$ZGDP.capita<-scale(d$GDP.capita)
d$ZGini<-scale(d$Gini)
d$ZMedian_Age<-scale(d$Median_Age)
d$ZUnderreporting<-0-scale(d$Underreporting)
d$ZDoctors<-scale(d$Healthcare)
d$Zlog_PopDensity<-scale(log(d$Population.density+1))
d$Authoritarianism<-ifelse(is.na(d$Authoritarian)==T,0,1)
d$ZAuthoritarianism<-scale(d$Authoritarianism)
d$Underreporting_certain<-(0-d$Underreporting_uncertain)+100
d$Days<-d$n
d$ZTests_Per_CaseOWD<-scale(d$Tests_Per_CaseOWD)

##Standardizing and re-centering tightness
d$ZTightness<-scale(d$Tightness)
d$ZTightness_l<-d$ZTightness+1
d$ZTightness_h<-d$ZTightness-1

##Standardizing and re-centering collectivism
d$ZCollectivism_c<-scale(d$Collectivism)
d$ZCollectivism_l<-d$ZCollectivism_c+1
d$ZCollectivism_h<-d$ZCollectivism_c-1

##Standardizing and re-centering power distance
d$ZPowerdistance_c<-scale(d$Powerdistance)
d$ZPowerdistance_l<-d$ZPowerdistance_c+1
d$ZPowerdistance_h<-d$ZPowerdistance_c-1

##Standardizing and re-centering relational mobility
d$ZRelationalMobility_c<-scale(d$RelationalMobility)
d$ZRelationalMobility_l<-d$ZRelationalMobility_c+1
d$ZRelationalMobility_h<-d$ZRelationalMobility_c-1

##Standardizing and re-centering gov efficiency
d$ZEfficiency<-scale(d$Government.efficiency)
d$ZEfficiency_l<-d$ZEfficiency+1
d$ZEfficiency_h<-d$ZEfficiency-1

nrow(na.omit(data.frame(d$Tightness,d$Government.efficiency, d$log_day,d$ZTests_Per_CaseOWD))) ##How many nations have data on our key variables?

cor.test(d$Tightness,d$TL33) ##correlation between original tightness measure and new tightness measure

############################################
##############Main Text Models##############
############################################

d2<-d[d$TLAlpha>.59,] ##subsetting based on within-nation reliability

#############Primary models#############

##Primary models: (OWD tests/cases, original efficiency measure)
summary(fit1a<-lm(log_day~Tightness_h*Government.efficiency_h+
                   ZTests_Per_CaseOWD,data=d,weights = Days))

summary(fit1b<-lm(log_day~Tightness_h*Government.efficiency_h+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

#############Secondary models#############

##Secondary models: (OWD tests/cases, original efficiency measure)
summary(fit2a<-lm(log_day~Tightness_h*Government.efficiency_h+Collectivism_c+Powerdistance_c+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d3,weights = Days))

summary(fit2b<-lm(log_day~Tightness_h*Government.efficiency_h+d$ZCollectivism_c*Government.efficiency_h+d$ZPowerdistance_c*Government.efficiency_h+
                    ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

#############Tertiary models#############

##Secondary models: (OWD tests/cases, original efficiency measure)
summary(fit3a<-lm(log_day~Tightness_h*Government.efficiency_h+
                    ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZUnderreporting,data=d,weights = Days))

summary(fit3b<-lm(log_day~Tightness_h*Government.efficiency_h+
                    ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZDoctors,data=d,weights = Days))

###########################################
############Supplemental Models############
###########################################

######Alternative Tightness Measures

summary(fitS1<-lm(log_day~Tightness_h*Government.efficiency_h+
                    ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d2,weights = Days))

summary(fitsS2<-lm(log_day~TL33_h*Government.efficiency_h+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

summary(fitsS3<-lm(log_day~CompletedTightness_h*Government.efficiency_h+
                    ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

######Government Effectiveness Analysis

summary(fitS4<-lm(log_day~ZTightness_h*Government.efficiency_h+Government.effectiveness.index_c+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

summary(fitS5<-lm(log_day~ZTightness_h*Government.efficiency_h+ZTightness_h*Government.effectiveness.index_c+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

summary(fitS6<-lm(log_day~ZTightness_h*Bur_Qual_h+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

summary(fitS7<-lm(log_day~ZTightness_h*WCY17GE_h+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

######Weighting based on underreporting uncertainty
summary(fitS8<-lm(log_day~Tightness_l*Government.efficiency_l+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZUnderreporting,data=d,weights = Underreporting_certain))

######Bootstrapping analysis

summary(fit1<-lm(log_day~Tightness_h*Government.efficiency_h+
                   ZTests_Per_CaseOWD,data=d))
summary(lm.boot(fit1,R=5000,rows=F))
confint(fit1)
hist(rnorm(5000,mean=-.44,sd=0.07))

summary(fit2<-lm(log_day~Tightness_h*Government.efficiency_h+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d))
summary(lm.boot(fit2,R=5000,rows=F))
confint(fit2)
hist(rnorm(5000,mean=-0.29,sd=0.08))

######Controlling for Spatial Autocorrelation

table(d$Continent)
d$Europe<-ifelse(d$Continent=="Europe",1,0)
d$Africa<-ifelse(d$Continent=="Africa",1,0)
d$Asia<-ifelse(d$Continent=="Asia",1,0)
d$SouthA<-ifelse(d$Continent=="South America",1,0)
d$NorthA<-ifelse(d$Continent=="North America",1,0)

##Primary models: (OWD tests/cases)
summary(fitS9<-lm(log_day~Tightness_h*Government.efficiency_h+ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+d$ZTests_Per_CaseOWD+
                   Europe+Africa+Asia+SouthA+NorthA,data=d,weights = Days))

######Model Diagnostics

##Testing for multicollinearity
summary(fit1<-lm(log_day~Tightness_h+Government.efficiency+ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = d$Days))
car::vif(fit1)

##Creating a diagnostics dataframe for testing
diag<-d[is.na(d$Tightness_h)==F,]
diag<-diag[is.na(diag$ZGDP.capita)==F,]
diag<-diag[is.na(diag$ZGini)==F,]
diag<-diag[is.na(diag$ZMedian_Age)==F,]
diag<-diag[is.na(diag$log_day)==F,]
diag<-diag[is.na(diag$ZTests_Per_CaseOWD)==F,]
diag<-diag[is.na(diag$Government.efficiency_h)==F,]

summary(fit1<-lm(log_day~Tightness_h*Government.efficiency_h+ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+d$ZTests_Per_CaseOWD,data=d,weights = Days))

diag$resid <- fit1$residuals #raw residuals
diag$predicted <- fit1$fitted.values #predicted values
diag$stud.resid <- rstudent(fit1) #standardized deleted residuals
diag$stand.resid <- rstandard(fit1) #standardized residuals
diag$hat <- hatvalues(fit1) #hat values

cor.test(diag$predicted,diag$resid)

#Heteroscedasticity test
ggplot(diag, aes(y = predicted, x = resid, label = Entity)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(method = "loess", se = T, color = "black") +
  geom_text(hjust = 0, nudge_x = .05)+theme_classic()

#Outlier test
car::outlierTest(fit1)
max(abs(diag$stud.resid))

##Creating a diagnostics dataframe for healthcare capacity
diag2<-d[is.na(d$TL)==F,]
diag2<-diag2[is.na(diag2$ZGDP.capita)==F,]
diag2<-diag2[is.na(diag2$ZGini)==F,]
diag2<-diag2[is.na(diag2$ZMedian_Age)==F,]
diag2<-diag2[is.na(diag2$log_day)==F,]
diag2<-diag2[is.na(diag2$ZDoctors)==F,]
diag2<-diag2[is.na(diag2$Government.efficiency_h)==F,]

diag2$resid <- fit2$residuals #raw residuals
diag2$predicted <- fit2$fitted.values #predicted values
diag2$stud.resid <- rstudent(fit2) #standardized deleted residuals
diag2$stand.resid <- rstandard(fit2) #standardized residuals
diag2$hat <- hatvalues(fit2) #hat values

#Heteroscedasticity test
ggplot(diag2, aes(y = predicted, x = resid, label = Entity)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(method = "loess", se = T, color = "black") +
  geom_text(hjust = 0, nudge_x = .05)+theme_classic()

#Outlier test
car::outlierTest(fit2)
max(abs(diag2$stud.resid))

##predicted values against residuals
cor.test(diag$predicted,diag$resid)
cor.test(diag2$predicted,diag2$resid)

d_noit<-d[d$Entity!="Singapore",]
summary(fit1<-lm(log_day~Tightness_h*Government.efficiency_h+ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d_noit,weights = Days))

######Controlling for Relational Mobility
summary(fitS10<-lm(log_day~Tightness_h*Government.efficiency_h+ZRelationalMobility_h+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

summary(fitS11<-lm(log_day~Tightness_h*Government.efficiency_h+ZRelationalMobility_h*Government.efficiency_h+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

######Controlling for Climate
summary(fitS12-lm(log_day~Tightness_h*Government.efficiency_h+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD+
                   scale(HeatStress)+scale(ColdStress)+scale(RainStress),data=d,weights = Days))

######Controlling for Authoritarianism
summary(fitS13<-lm(log_day~Tightness_h*Government.efficiency_h+ZAuthoritarianism+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZTests_Per_CaseOWD,data=d,weights = Days))

#################################
##For plotting estimated curves##
#################################

library(ggplot2)

curves<-read.csv("~/Desktop/University of Maryland/Maryland Manuscripts/Active/COVID Growth/Nature Human Behavior/Revision/Code and Data to Upload/for curves.csv")

##Remove the dashed lines for no SE intervals
ggplot(curves,aes(x=Day))+
  geom_point(aes(y=LL_Label),color="darkslategray4")+
  geom_point(aes(y=MM_Label),color="gray35")+
  geom_point(aes(y=HH_Label),color="firebrick3")+
  stat_smooth(aes(y=LL_Rate),geom="line",method="loess",color="darkslategray4",alpha=.50)+
  stat_smooth(aes(y=MM_Rate),geom="line",method="loess",color="gray35",alpha=.50)+
  stat_smooth(aes(y=HH_Rate),geom="line",method="loess",color="firebrick3",alpha=.50)+
  stat_smooth(aes(y=LL_Rate_low),geom="line",method="loess",color="darkslategray4",alpha=.50,linetype="dashed")+
  stat_smooth(aes(y=MM_Rate_low),geom="line",method="loess",color="gray35",alpha=.50,linetype="dashed")+
  stat_smooth(aes(y=HH_Rate_low),geom="line",method="loess",color="firebrick3",alpha=.50,linetype="dashed")+
  stat_smooth(aes(y=LL_Rate_high),geom="line",method="loess",color="darkslategray4",alpha=.50,linetype="dashed")+
  stat_smooth(aes(y=MM_Rate_high),geom="line",method="loess",color="gray35",alpha=.50,linetype="dashed")+
  stat_smooth(aes(y=HH_Rate_high),geom="line",method="loess",color="firebrick3",alpha=.50,linetype="dashed")+
  theme_classic()+
  geom_text(aes(label=LL_Label,y=LL_Label),nudge_x = -2.5)+
  geom_text(aes(label=MM_Label,y=MM_Label),nudge_x = -2.5)+
  geom_text(aes(label=HH_Label,y=HH_Label),nudge_x = -2.5)

######################################################
#################Analysis of tightness################
######################################################

library(lavaan)

tl<-read.csv("~/Desktop/individual tightness data.csv")

tl$PMean<-tl$Tightness_Q1-tl$Tightness_Q1_adjusted
tl$Tightness_Q4_adjusted2<-tl$Tightness_Q4_adjusted-2*tl$PMean ##Mean centering the 4th item (mean was originally added rather than subtracted)

tl$ZTightness_Q1<-scale(tl$Tightness_Q1_adjusted)
tl$ZTightness_Q2<-scale(tl$Tightness_Q2_adjusted)
tl$ZTightness_Q3<-scale(tl$Tightness_Q3_adjusted)
tl$ZTightness_Q4<-scale(tl$Tightness_Q4_adjusted2)
tl$ZTightness_Q5<-scale(tl$Tightness_Q5_adjusted)
tl$ZTightness_Q6<-scale(tl$Tightness_Q6_adjusted)

tl2<-tl[is.na(tl$Age)==F,]  ##excluding imputed values
tl3<-tl2[tl2$Attention==4,] ##excluding people who failed the attention check
tl3b<-tl[is.na(tl$Attention)==F,] ##excluding people who failed the attention check but leaving in imputed values

tlitems<-data.frame(tl3$ZTightness_Q1,
                    tl3$ZTightness_Q2,
                    tl3$ZTightness_Q3,
                    tl3$ZTightness_Q4,
                    tl3$ZTightness_Q5,
                    tl3$ZTightness_Q6)

psych::alpha(tlitems)

tl4<-tl3[tl3$SiteCountry!="Algeria"&tl3$SiteCountry!="Kazakhstan"&tl3$SiteCountry!="Botswana"&       ##nations with lower reliability
                   tl3$SiteCountry!="Iran"&tl3$SiteCountry!="Slovakia"&tl3$SiteCountry!="Armenia",]

mod <- '
        level: 1
            tl =~ ZTightness_Q1 + ZTightness_Q2 + ZTightness_Q3 + ZTightness_Q4 + ZTightness_Q5 + ZTightness_Q6
        level: 2
            ZTightness_Q1 ~~ ZTightness_Q2 + ZTightness_Q3 + ZTightness_Q4 + ZTightness_Q5 + ZTightness_Q6
            ZTightness_Q2 ~~ ZTightness_Q3 + ZTightness_Q4 + ZTightness_Q5 + ZTightness_Q6
            ZTightness_Q3 ~~ ZTightness_Q4 + ZTightness_Q5 + ZTightness_Q6
            ZTightness_Q4 ~~ ZTightness_Q5 + ZTightness_Q6
            ZTightness_Q5 ~~ ZTightness_Q6
    '

fit <- sem(model = mod, data = tl3, missing = "ml", cluster = "SiteCountry")
summary(fit, fit.measures = T, standardized = T, rsquare = T,ci=T)
