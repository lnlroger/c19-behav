
library(tidyr)
library(dplyr)
library(stringr)
library(lmerTest)
library(stargazer)
library(ggplot2)
library(mfx)
library(knitr)
library(kSamples)
library(FSA)
library(kableExtra)
library(xtable)
library(miceadds)
library(jtools)
library(plm)
library(clubSandwich)
library(ggrepel)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(naniar)
library(glmnet)

options("jtools-digits" = 4,scipen=999) # set displayed decimal places to 4

df<-read.csv(here::here("24042020_short.csv"))
dfl<-read.csv(here::here("22042020_long.csv"))

# Elastic net ----

vars_best<-c("log_GDP_pc","DaysDuration","Days_LockDown_1death","ROL","HumidityDif","Humidity",
             "TemperatureDif","Temperature","Unemployment","Death_disease","Pop_Density_sq_mi","polity2")

vars_intr<-c("log_GDP_pc","DaysDuration","Days_LockDown_1death",
             "ROL","HumidityDif","Humidity","TemperatureDif","Temperature",
             "Unemployment","Pop_Density_sq_mi","risktaking","patience","ROL","polity2")

use.cases <- which(complete.cases(df[c("Movement",vars_best)]))

elastic <- cv.glmnet(as.matrix(df[use.cases,vars_best]),df$Movement[use.cases])
plot(elastic)
small.lambda.index <- which(elastic$lambda == elastic$lambda.min)
small.lambda.betas <- elastic$glmnet.fit$beta[, small.lambda.index]
small.lambda.betas

# Average response by regime type

df <- df %>%
  mutate(regime = ifelse(polity2 <= -6,"autocracy",
                         ifelse(polity2 <= 5,"anocracy",
                                ifelse(polity2 > 5,"democracy",NA))))

dfl <- dfl %>%
  left_join(dplyr::select(df,regime,Country))

movement.regime <- dplyr::select(dfl,Country,Date,Movement,regime) %>%
  group_by(regime,Date) %>%
  summarise(mean.movement = mean(Movement,na.rm = TRUE))
  

plot.movement.regime <- ggplot(data = movement.regime, mapping = aes(Date,mean.movement,group = regime)) +
  geom_line(aes(colour = regime)) +
  theme_minimal()
plot.movement.regime

dfl <- arrange(dfl,Country,date.x)
plot.movement <- ggplot(data = dfl, mapping = aes(date.x,Movement,group = Country)) +
  geom_line(colour = "grey91") +
  theme(legend.position = "none") +
  theme_minimal()
plot.movement


