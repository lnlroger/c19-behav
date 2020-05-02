
################################################################################################
#####Institutional and Cultural Factors Predicting Infection Rates and Mortality of COVID-19####
###############################Analysis of Infection Rates######################################
#########################Direct questions to joshcj@live.unc.edu################################
##########################Original Code: https://osf.io/pc4ef/##################################
###############Modified by Michael Muthukrishna (m.muthukrishna@lse.ac.uk)######################
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

library(readstata13)
library(stargazer)

##Estimating the nation-specific exponential growth curves

case<-read.csv("covid-confirmed-cases-per-million-since-1-per-million.csv")
case<-case[case$Entity=="",] ##Enter nation of interest here
case<-case[is.na(case$Days.since.the.total.confirmed.cases.of.COVID.19.per.million.people.reached.1)==F,]

case$logcase<-log(case$Total.confirmed.cases.of.COVID.19.per.million.people..cases.per.million.)
case$logdays<-log(case$Days.since.the.total.confirmed.cases.of.COVID.19.per.million.people.reached.1+1)

summary(lm(logcase~logdays,data=case))

###Factors predicting infection rate
d<-read.csv("infectionrate_dat.csv")
cnt_dat <- read.csv("country_continent.csv")

##My log models
hist(d$CaseLog)

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

#Fix country names
d$Entity <- as.character(d$Entity)
d$Entity[d$Entity=="Argenti"] <- "Argentina"
d$Entity[d$Entity=="Burki Faso"] <- "Burkina Faso"
d$Entity[d$Entity=="Botswa"] <- "Botswana"
d$Entity[d$Entity=="Bosnia and Herzegovi"] <- "Bosnia and Herzegovina"
d$Entity[d$Entity=="Cada"] <- "Canada"
d$Entity[d$Entity=="Chi"] <- "China"
d$Entity[d$Entity=="Czech Republic"] <- "Czechia"
d$Entity[d$Entity=="Gha"] <- "Ghana"
d$Entity[d$Entity=="mibia"] <- "Namibia"
d$Entity[d$Entity=="Surime"] <- "Suriname"
d$Entity[d$Entity=="Vietm"] <- "Viet Nam"
d$Entity[d$Entity=="Bolivia (Plurinational State of)"] <- "Bolivia"
d$Entity[d$Entity=="Czech Republic"] <- "Czechia"
d$Entity[d$Entity=="Gambia, The"] <- "Gambia"
d$Entity[d$Entity=="Moldova, Republic of"] <- "Moldova"
d$Entity[d$Entity=="Russian Federation"] <- "Russia"
d$Entity[d$Entity=="Korea, Republic of"] <- "South Korea"
d$Entity[d$Entity=="Taiwan, Province of China"] <- "Taiwan"
d$Entity[d$Entity=="Tanzania, United Republic of"] <- "Tanzania"
d$Entity[d$Entity=="United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
d$Entity[d$Entity=="United States of America"] <- "United States"
d$Entity[d$Entity=="Venezuela (Bolivarian Republic of)"] <- "Venezuela"
d$Entity[d$Entity=="Vietnam"] <- "Viet Nam"
d$Entity[d$Entity=="Brunei Darussalam"] <- "Brunei"
d$Entity[d$Entity=="Iran (Islamic Republic of)"] <- "Iran"


cnt_dat$name <- as.character(cnt_dat$name)
cnt_dat$name[cnt_dat$name=="Bolivia (Plurinational State of)"] <- "Bolivia"
cnt_dat$name[cnt_dat$name=="Czech Republic"] <- "Czechia"
cnt_dat$name[cnt_dat$name=="Gambia, The"] <- "Gambia"
cnt_dat$name[cnt_dat$name=="Moldova, Republic of"] <- "Moldova"
cnt_dat$name[cnt_dat$name=="Russian Federation"] <- "Russia"
cnt_dat$name[cnt_dat$name=="Korea, Republic of"] <- "South Korea"
cnt_dat$name[cnt_dat$name=="Taiwan, Province of China"] <- "Taiwan"
cnt_dat$name[cnt_dat$name=="Tanzania, United Republic of"] <- "Tanzania"
cnt_dat$name[cnt_dat$name=="United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
cnt_dat$name[cnt_dat$name=="United States of America"] <- "United States"
cnt_dat$name[cnt_dat$name=="Venezuela (Bolivarian Republic of)"] <- "Venezuela"
cnt_dat$name[cnt_dat$name=="Vietnam"] <- "Viet Nam"
cnt_dat$name[cnt_dat$name=="Brunei Darussalam"] <- "Brunei"
cnt_dat$name[cnt_dat$name=="Iran (Islamic Republic of)"] <- "Iran"

d <- merge(d, cnt_dat, by.x="Entity", by.y="name", all=T)
##Main Effects
summary(lm(CaseLog~TL+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days))
summary(lm(CaseLog~Government.efficiency+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days))

m1 <- lm(CaseLog~TL,data=d,weights = Days)
m2 <- lm(CaseLog~TL+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days)
m3 <- lm(CaseLog~Government.efficiency,data=d,weights = Days)
m4 <- lm(CaseLog~Government.efficiency+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days)

m1.c <- lm(CaseLog~TL+region,data=d,weights = Days)
m2.c <- lm(CaseLog~TL+ZGDP.capita+ZGini+ZMedian_Age+region,data=d,weights = Days)
m3.c <- lm(CaseLog~Government.efficiency+region,data=d,weights = Days)
m4.c <- lm(CaseLog~Government.efficiency+ZGDP.capita+ZGini+ZMedian_Age+region,data=d,weights = Days)


d_tl<-d[is.na(d$TL)==F,]
d_tl<-d_tl[is.na(d_tl$ZGDP.capita)==F,]
d_tl<-d_tl[is.na(d_tl$ZGini)==F,]
d_tl<-d_tl[is.na(d_tl$ZMedian_Age)==F,]
d_tl<-d_tl[is.na(d_tl$CaseLog)==F,]

d_tl$TL_resid<-lm(TL~ZGDP.capita+ZGini+ZMedian_Age,data=d_tl)$residuals
d_tl$CaseLog_resid<-lm(CaseLog~ZGDP.capita+ZGini+ZMedian_Age,data=d_tl)$residuals
d_tl$CaseLog_resid<-d_tl$CaseLog_resid+abs(min(d_tl$CaseLog_resid))+1

ggplot(data=d_tl,aes(x=TL_resid, y=CaseLog_resid,label=Entity)) +
  geom_point(aes(size=Days),color="coral3")+
  geom_smooth(method="lm",color="black",se=F) +
  theme_classic()+
  geom_text(aes(label=Entity),hjust=-.15, vjust=-.05)

d_gov<-d[is.na(d$Government.efficiency)==F,]
d_gov<-d_gov[is.na(d_gov$GDP.capita)==F,]
d_gov<-d_gov[is.na(d_gov$Gini)==F,]
d_gov<-d_gov[is.na(d_gov$Median_Age)==F,]
d_gov<-d_gov[is.na(d_gov$CaseLog)==F,]

d_gov$gov_resid<-lm(Government.efficiency~GDP.capita+Gini+Median_Age,data=d_gov)$residuals
d_gov$CaseLog_resid<-lm(CaseLog~GDP.capita+Gini+Median_Age,data=d_gov)$residuals
d_gov$CaseLog_resid<-d_gov$CaseLog_resid+abs(min(d_gov$CaseLog_resid))+1

ggplot(data=d_gov,aes(x=gov_resid, y=CaseLog_resid,label=Entity)) +
  geom_point(aes(size=Days),color="aquamarine4")+
  geom_smooth(method="lm",color="black",se=F) +
  theme_classic()+
  geom_text(aes(label=Entity),hjust=-.15, vjust=-.05)

##Interactions
summary(lm(CaseLog~TL*Government.efficiency+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days))
summary(lm(CaseLog~TL_c*Government.efficiency_h+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days))
summary(lm(CaseLog~TL_c*Government.efficiency_l+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days))
summary(lm(CaseLog~TL_h*Government.efficiency_c+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days))
summary(lm(CaseLog~TL_l*Government.efficiency_c+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days))

summary(lm(CaseLog~TL_c*Government.efficiency_c+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days))
summary(lm(CaseLog~TL_l*Government.efficiency_l+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days))
summary(lm(CaseLog~TL_h*Government.efficiency_h+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days))

m5 <- lm(CaseLog~TL*Government.efficiency,data=d,weights = Days)
m6 <- lm(CaseLog~TL*Government.efficiency+ZGDP.capita+ZGini+ZMedian_Age,data=d,weights = Days)

m5.c <- lm(CaseLog~TL*Government.efficiency+region,data=d,weights = Days)
m6.c <- lm(CaseLog~TL*Government.efficiency+ZGDP.capita+ZGini+ZMedian_Age+region,data=d,weights = Days)

##Alternative covariates (density and GDP)
d$ZlogDensity<-scale(log(d$Population.density+1))
cor.test(d$Population.density,d$CaseLog)

summary(lm(CaseLog~TL+ZGDP.capita+ZlogDensity,data=d,weights = Days))
summary(lm(CaseLog~Government.efficiency+ZGDP.capita+ZlogDensity,data=d,weights = Days))
summary(lm(CaseLog~Government.efficiency*TL+ZGDP.capita+ZlogDensity,data=d,weights = Days))

##for plotting estimated curves
curves<-read.csv("for curves.csv")

ggplot(curves,aes(x=Day))+
  geom_point(aes(y=Slow_Label),color="darkslategray4")+
  geom_point(aes(y=Medium_Label),color="gray35")+
  geom_point(aes(y=High_Label),color="firebrick3")+
  stat_smooth(aes(y=Slow_Rate),geom="line",method="loess",color="darkslategray4",alpha=.50)+
  stat_smooth(aes(y=Medium_Rate),geom="line",method="loess",color="gray35",alpha=.50)+
  stat_smooth(aes(y=High_Rate),geom="line",method="loess",color="firebrick3",alpha=.50)+
  theme_classic()+
  geom_text(aes(label=Slow_Label,y=Slow_Label),nudge_x = -2.5)+
  geom_text(aes(label=Medium_Label,y=Medium_Label),nudge_x = -2.5)+
  geom_text(aes(label=High_Label,y=High_Label),nudge_x = -2.5)

# Muthukrishna new analyses ----
## Adding collectivism 
hofstede <- read.csv("hofstede.csv")
d$Entity <- as.character(d$Entity)
hofstede$Country <- as.character(hofstede$Country)
hofstede$Country[hofstede$Country=="Czech Republic"] <- "Czechia"

d_hof <- merge(d, hofstede, by.x = "Entity", by.y = "Country", all = T)

d_hof$COL <- 100-d_hof$IDV
d_hof$zCOL <- scale(d_hof$COL)

summary(lm(CaseLog~zCOL+ZGDP.capita+ZGini+ZMedian_Age,data=d_hof,weights = Days))
summary(lm(CaseLog~TL+zCOL+ZGDP.capita+ZGini+ZMedian_Age,data=d_hof,weights = Days))
summary(lm(CaseLog~Government.efficiency+zCOL+ZGDP.capita+ZGini+ZMedian_Age,data=d_hof,weights = Days))

m7 <- lm(CaseLog~zCOL,data=d_hof,weights = Days)
m8 <- lm(CaseLog~zCOL+ZGDP.capita+ZGini+ZMedian_Age,data=d_hof,weights = Days)
m9 <- lm(CaseLog~TL+zCOL+ZGDP.capita+ZGini+ZMedian_Age,data=d_hof,weights = Days)
m10 <- lm(CaseLog~Government.efficiency+zCOL+ZGDP.capita+ZGini+ZMedian_Age,data=d_hof,weights = Days)

m7.c <- lm(CaseLog~zCOL+region,data=d_hof,weights = Days)
m8.c <- lm(CaseLog~zCOL+ZGDP.capita+ZGini+ZMedian_Age+region,data=d_hof,weights = Days)
m9.c <- lm(CaseLog~TL+zCOL+ZGDP.capita+ZGini+ZMedian_Age+region,data=d_hof,weights = Days)
m10.c <- lm(CaseLog~Government.efficiency+zCOL+ZGDP.capita+ZGini+ZMedian_Age+region,data=d_hof,weights = Days)

summary(lm(CaseLog~TL*zCOL+ZGDP.capita+ZGini+ZMedian_Age,data=d_hof,weights = Days))
summary(lm(CaseLog~Government.efficiency*zCOL+ZGDP.capita+ZGini+ZMedian_Age,data=d_hof,weights = Days))

summary(lm(CaseLog~TL*Government.efficiency+zCOL+ZGDP.capita+ZGini+ZMedian_Age,data=d_hof,weights = Days))

#Add education
vdem <- read.dta13("V-Dem-CY-Full+Others-v10.dta") #Available at: https://www.v-dem.net/en/
vdem.2019 <- vdem[vdem$year==2019,]
vdem.2019$country_name <- as.character(vdem.2019$country_name)
vdem.2019$country_name[vdem.2019$country_name=="Bolivia (Plurinational State of)"] <- "Bolivia"
vdem.2019$country_name[vdem.2019$country_name=="Brunei Darussalam"] <- "Brunei"
vdem.2019$country_name[vdem.2019$country_name=="Czech Republic"] <- "Czechia"
vdem.2019$country_name[vdem.2019$country_name=="Iran (Islamic Republic of)"] <- "Iran"
vdem.2019$country_name[vdem.2019$country_name=="Gambia, The"] <- "Gambia"
vdem.2019$country_name[vdem.2019$country_name=="Moldova, Republic of"] <- "Moldova"
vdem.2019$country_name[vdem.2019$country_name=="Russian Federation"] <- "Russia"
vdem.2019$country_name[vdem.2019$country_name=="Korea, Republic of"] <- "South Korea"
vdem.2019$country_name[vdem.2019$country_name=="Taiwan, Province of China"] <- "Taiwan"
vdem.2019$country_name[vdem.2019$country_name=="Tanzania, United Republic of"] <- "Tanzania"
vdem.2019$country_name[vdem.2019$country_name=="United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
vdem.2019$country_name[vdem.2019$country_name=="United States of America"] <- "United States"
vdem.2019$country_name[vdem.2019$country_name=="Venezuela (Bolivarian Republic of)"] <- "Venezuela"
vdem.2019$country_name[vdem.2019$country_name=="Vietnam"] <- "Viet Nam"
d_hof$Entity <- as.character(d_hof$Entity)
d_hof_vdem <- merge(d_hof, vdem.2019, by.x = "Entity", by.y = "country_name", all = T)

summary(lm(CaseLog~zCOL+ZGDP.capita+ZGini+ZMedian_Age+e_peaveduc,data=d_hof_vdem,weights = Days))
summary(lm(CaseLog~TL+zCOL+ZGDP.capita+ZGini+ZMedian_Age+e_peaveduc,data=d_hof_vdem,weights = Days))
summary(lm(CaseLog~Government.efficiency+zCOL+ZGDP.capita+ZGini+ZMedian_Age+e_peaveduc,data=d_hof_vdem,weights = Days))

m11 <- lm(CaseLog~zCOL+ZGDP.capita+ZGini+ZMedian_Age+e_peaveduc,data=d_hof_vdem,weights = Days)
m12 <- lm(CaseLog~TL+zCOL+ZGDP.capita+ZGini+ZMedian_Age+e_peaveduc,data=d_hof_vdem,weights = Days)
m13 <- lm(CaseLog~Government.efficiency+zCOL+ZGDP.capita+ZGini+ZMedian_Age+e_peaveduc,data=d_hof_vdem,weights = Days)

m11.c <- lm(CaseLog~zCOL+ZGDP.capita+ZGini+ZMedian_Age+e_peaveduc+region,data=d_hof_vdem,weights = Days)
m12.c <- lm(CaseLog~TL+zCOL+ZGDP.capita+ZGini+ZMedian_Age+e_peaveduc+region,data=d_hof_vdem,weights = Days)
m13.c <- lm(CaseLog~Government.efficiency+zCOL+ZGDP.capita+ZGini+ZMedian_Age+e_peaveduc+region,data=d_hof_vdem,weights = Days)


cor.test(d_hof$IDV, d_hof$Government.efficiency)
stargazer(m1,m2,m3,m4,m5,m6)

stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,
          digits=2,
          ci=T,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          type="html",
          out="gelfand_plus.doc")

stargazer(m1.c,m2.c,m3.c,m4.c,m5.c,m6.c,m7.c,m8.c,m9.c,m10.c,m11.c,m12.c,m13.c,
          digits=2,
          ci=T,
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          type="html",
          out="gelfand_plus_continent.doc")


## Figures
### Correlation table
library("PerformanceAnalytics")
d_cor <- d_hof
d_cor$Tightness <- d_cor$TL
d_cor$Collectivism <- d_cor$COL
d_cor <- d_cor[c("CaseLog","Collectivism", "Tightness", "Government.efficiency")]
chart.Correlation(d_cor, histogram=TRUE, pch=19)
ggpairs(d_cor, columns=c("CaseLog","Collectivism", "Tightness", "Government.efficiency"))

library(GGally)   
# Matrix of plots
p1 <- ggpairs(d_cor, lower = list(continuous = "smooth"))
# Correlation matrix plot
p2 <- ggcorr(d_cor, label = TRUE, label_round = 2)

# Get list of colors from the correlation matrix plot
g2 <- ggplotGrob(p2)
colors <- g2$grobs[[6]]$children[[3]]$gp$fill

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
for (k1 in 1:3) {
  for (k2 in (k1+1):4) {
    plt <- getPlot(p1,k1,k2) +
      theme(panel.background = element_rect(fill = colors[idx], color="white"),
            panel.grid.major = element_line(color=colors[idx]))
    p1 <- putPlot(p1,plt,k1,k2)
    idx <- idx+1
  }
}
print(p1)


ggsave("corr_plot.png", p1, height = 6, width=6)


# Just collectivism and Case Log
library(ggpubr)
(p2 <- ggscatterhist(d_hof, x="COL", y="CaseLog",
              #size = "Population.density",
              #color = "blacl",
              #shape = 1,
              label = "Entity",
              repel = T,
              add = "reg.line",
              add.params = list(fill = "lightgray"),
              conf.int = TRUE,
              cor.coef = TRUE,
              fullrange = TRUE,
              cor.coeff.args = list(method = "pearson", label.x = -3.5, label.y = .3, label.sep = "\n", size = 6),
              xlab = "Collectivism",
              margin.params = list(fill = "lightgray")
              
)
)

ggsave("corr_col_plot.png", p2, height = 6, width=6)
  
