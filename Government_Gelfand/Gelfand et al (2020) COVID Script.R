
################################################################################################
#####Institutional and Cultural Factors Predicting Infection Rates and Mortality of COVID-19####
################################################################################################

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

#####################################
##Factors predicting infection rate##
#####################################

d1<-read.csv("infectionrate_dat2.csv")
d2<-read.csv("growth.coefficients.csv")

d<-merge(d1,d2,by="Entity",all=T)

##Distribution
hist(d$log_day)

#Centering variables
d$TL_c<-d$TL-mean(d$TL,na.rm=T)
d$TL_l<-d$TL-(mean(d$TL,na.rm=T)-sd(d$TL,na.rm=T))
d$TL_h<-d$TL-(mean(d$TL,na.rm=T)+sd(d$TL,na.rm=T))
d$TL_2<-d$TL_c*d$TL_c

d$Government.efficiency_c<-d$Government.efficiency-mean(d$Government.efficiency,na.rm=T)
d$Government.efficiency_l<-d$Government.efficiency-(mean(d$Government.efficiency,na.rm=T)-sd(d$Government.efficiency,na.rm=T))
d$Government.efficiency_h<-d$Government.efficiency-(mean(d$Government.efficiency,na.rm=T)+sd(d$Government.efficiency,na.rm=T))
d$Government.efficiency_2<-d$Government.efficiency_c*d$Government.efficiency_c

##Standardizing covariates
d$ZGDP.capita<-scale(d$GDP.capita)
d$ZGini<-scale(d$Gini)
d$ZMedian_Age<-scale(d$Median_Age)
d$ZUnderreporting<-0-scale(d$Underreporting)
d$Zlog_PopDensity<-scale(log(d$Population.density+1))
d$Authoritarianism<-ifelse(is.na(d$Authoritarian)==T,0,1)
d$ZAuthoritarianism<-scale(d$Authoritarianism)

nrow(na.omit(data.frame(d$TL,d$Government.efficiency,d$Underreporting))) ##How many nations have data on our key variables?

##Primary models
summary(fit1<-lm(log_day~TL_c*Government.efficiency_c+ZUnderreporting,data=d,weights = Days))
summary(fit2<-lm(log_day~TL_c*Government.efficiency_c+ZUnderreporting+ZGDP.capita+ZGini,data=d,weights = Days))
summary(fit3<-lm(log_day~TL_c*Government.efficiency_c+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZAuthoritarianism+ZUnderreporting,data=d,weights = Days))

##Without day weights for bootstrapping
summary(fit1<-lm(log_day~TL_h*Government.efficiency_h+
                   ZUnderreporting,data=d))
summary(boot1<-lm.boot(fit1,R=5000,rows=F))
confint(fit1)
hist(rnorm(5000,mean=-0.1683,sd=0.07715181))

summary(fit2<-lm(log_day~TL_h*Government.efficiency_h+
                   ZGDP.capita+ZGini+ZUnderreporting,data=d))
summary(lm.boot(fit2,R=5000,rows=F))
confint(fit2)
hist(rnorm(5000,mean=-0.2745,sd=0.07304245))

summary(fit3<-lm(log_day~TL_c*Government.efficiency_c+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZAuthoritarianism+ZUnderreporting,data=d))
summary(lm.boot(fit3,R=5000,rows=F))
confint(fit3)
hist(rnorm(5000,mean=-0.329341,sd=0.06971553 ))

##Controlling for any spatial interdependence
table(d$Continent)
d$Europe<-ifelse(d$Continent=="Europe",1,0)
d$Africa<-ifelse(d$Continent=="Africa",1,0)
d$Asia<-ifelse(d$Continent=="Asia",1,0)
d$SouthA<-ifelse(d$Continent=="South America",1,0)
d$NorthA<-ifelse(d$Continent=="North America",1,0)

summary(fit4<-lm(log_day~TL_c*Government.efficiency_c+ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+Authoritarianism+ZUnderreporting+
                   Europe+Africa+Asia+SouthA+NorthA,data=d,weights = Days))

##Model diagnostics

##Testing for multicollinearity
summary(fit5<-lm(log_day~TL+Government.efficiency+ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+Authoritarianism+ZUnderreporting,data=d,weights = d$Days))
car::vif(fit5)

##Creating a diagnostics dataframe
diag<-d[is.na(d$TL)==F,]
diag<-diag[is.na(diag$ZGDP.capita)==F,]
diag<-diag[is.na(diag$ZGini)==F,]
diag<-diag[is.na(diag$ZMedian_Age)==F,]
diag<-diag[is.na(diag$log_day)==F,]
diag<-diag[is.na(diag$ZUnderreporting)==F,]
diag<-diag[is.na(diag$Government.efficiency_h)==F,]

diag$resid <- fit3$residuals #raw residuals
diag$predicted <- fit3$fitted.values #predicted values
diag$stud.resid <- rstudent(fit3) #standardized deleted residuals
diag$stand.resid <- rstandard(fit3) #standardized residuals
diag$hat <- hatvalues(fit3) #hat values

#Heteroscedasticity test
ggplot(diag, aes(y = predicted, x = resid, label = Entity)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_text(hjust = 0, nudge_x = .05)+theme_classic()

#Outlier test
outlierTest(fit3)
max(abs(diag$stud.resid))

#d_nos<-d[d$Entity!="Singapore",]
summary(fit3<-lm(log_day~TL_h*Government.efficiency_h+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZAuthoritarianism+ZUnderreporting,data=d,weights = Days))

#################################
##For plotting estimated curves##
#################################

curves<-read.csv("for curves (April 4).csv")

ggplot(curves,aes(x=Day))+
  geom_point(aes(y=LL_Label),color="darkslategray4")+
  geom_point(aes(y=MM_Label),color="gray35")+
  geom_point(aes(y=HH_Label),color="firebrick3")+
  stat_smooth(aes(y=LL_Rate),geom="line",method="loess",color="darkslategray4",alpha=.50)+
  stat_smooth(aes(y=MM_Rate),geom="line",method="loess",color="gray35",alpha=.50)+
  stat_smooth(aes(y=HH_Rate),geom="line",method="loess",color="firebrick3",alpha=.50)+
  theme_classic()+
  geom_text(aes(label=LL_Label,y=LL_Label),nudge_x = -2.5)+
  geom_text(aes(label=MM_Label,y=MM_Label),nudge_x = -2.5)+
  geom_text(aes(label=HH_Label,y=HH_Label),nudge_x = -2.5)

#############################################
###Factors predicting mortality likelihood###
#############################################

d<-read.csv("mortality_likelihood_dat.csv")
  
hist(d$Mortality.Likelihood) ##Skewed distribution
d$log_mortallike<-log10(d$Mortality.Likelihood+1)
d$log_mortallike<-d$log_mortallike+abs(min(d$log_mortallike,na.rm=T)+.01) ##Less skewed but still not normal. Suggests that we should validate our results with a generalized linear model
d$ZUnderreporting<-0-d$ZUnderreporting #Underreporting is reverse scored
d$ZAuthoritarianism<-scale(d$Authoritarianism)

#Primary models
summary(fit1<-lm(log_mortallike~TL_c*Government.efficiency_c+ZUnderreporting,data=d))
summary(fit2<-lm(log_mortallike~TL_c*Government.efficiency_c+ZUnderreporting+ZGDP.capita+ZGini,data=d))
summary(fit3<-lm(log_mortallike~TL_c*Government.efficiency_c+
             ZUnderreporting+ZGDP.capita+ZGini+Zlog_PopDensity+scale(Authoritarianism)+ZMedian_Age,data=d))

#Replicating with the raw scores to extract percentages
summary(fit4<-lm(Mortality.Likelihood~TL_h*Government.efficiency_h+ZUnderreporting,data=d))
summary(fit5<-lm(Mortality.Likelihood~TL_h*Government.efficiency_h+ZUnderreporting+ZGDP.capita+ZGini,data=d))
summary(fit6<-lm(Mortality.Likelihood~TL_l*Government.efficiency_l+
             ZUnderreporting+ZGDP.capita+ZGini+Zlog_PopDensity+scale(Authoritarianism)+ZMedian_Age,data=d))

#Replicating with a generalized linear model approach that does not assume normal distribution
d$Mortality.Likelihood2<-d$Mortality.Likelihood+.001 #linear transformation for the generalized linear model

summary(fit7<-glm(Mortality.Likelihood2~TL_c*Government.efficiency_c+ZUnderreporting,family=Gamma(link="logit"),data=d))
summary(fit8<-glm(Mortality.Likelihood2~TL_l*Government.efficiency_l+ZUnderreporting+ZGDP.capita+ZGini,family=Gamma(link="logit"),data=d))
summary(fit9<-glm(Mortality.Likelihood2~TL_h*Government.efficiency_h+
             ZUnderreporting+ZGDP.capita+ZGini+Zlog_PopDensity+scale(Authoritarianism)+ZMedian_Age,family=Gamma(link="logit"),data=d))

##Bootstrapping
summary(fit1<-lm(log_mortallike~TL_h*Government.efficiency_h+
                   ZUnderreporting,data=d))
summary(boot1<-lm.boot(fit1,R=5000,rows=F))
confint(fit1)
hist(rnorm(5000,mean=-0.003493,sd=0.001444990))

summary(fit2<-lm(log_mortallike~TL_h*Government.efficiency_h+
                   ZGDP.capita+ZGini+ZUnderreporting,data=d))
summary(lm.boot(fit2,R=5000,rows=F))
confint(fit2)
hist(rnorm(5000,mean=-0.0038506,sd=0.001561989))

summary(fit3<-lm(log_mortallike~TL_h*Government.efficiency_h+
                   ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+ZAuthoritarianism+ZUnderreporting,data=d))
summary(lm.boot(fit3,R=5000,rows=F))
confint(fit3)
hist(rnorm(5000,mean=-0.0045117,sd=0.001670244))

##Diagnostics

##Testing for multicollinearity
summary(fit1<-lm(log_mortallike~TL+Government.efficiency+ZGDP.capita+ZGini+ZMedian_Age+Zlog_PopDensity+Authoritarianism+ZUnderreporting,data=d,weights = d$Days))
car::vif(fit1)

##Creating a diagnostics dataframe
diag<-d[is.na(d$TL)==F,]
diag<-diag[is.na(diag$ZGDP.capita)==F,]
diag<-diag[is.na(diag$ZGini)==F,]
diag<-diag[is.na(diag$ZMedian_Age)==F,]
diag<-diag[is.na(diag$log_day)==F,]
diag<-diag[is.na(diag$ZUnderreporting)==F,]
diag<-diag[is.na(diag$Government.efficiency_h)==F,]

diag$resid <- fit3$residuals #raw residuals
diag$predicted <- fit3$fitted.values
diag$stud.resid <- rstudent(fit3) #standardized deleted residuals
diag$stand.resid <- rstandard(fit3) #standardized residuals
diag$hat <- hatvalues(fit3) #hat values

ggplot(diag, aes(y = predicted, x = resid, label = Entity)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_text(hjust = 0, nudge_x = .00)+theme_classic()

outlierTest(fit1)

##d_noa<-d[d$Entity!="Algeria",]

summary(glm(log_mortallike~TL_h*Government.efficiency_h+
              ZUnderreporting+ZGDP.capita+ZGini+Zlog_PopDensity+scale(Authoritarianism)+ZMedian_Age,family=Gamma(link="logit"),data=d))

