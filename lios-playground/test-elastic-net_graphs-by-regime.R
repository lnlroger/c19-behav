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

df<-read.csv(here::here("02052020_short.csv"))
dfl<-read.csv(here::here("02052020_long.csv"))

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

movement.regime <- dplyr::select(dfl,Country,Date,Movement,regime,DaysSinceLockdown) %>%
  group_by(regime,DaysSinceLockdown) %>%
  summarise(mean.movement = mean(Movement,na.rm = TRUE)) %>%
  filter((DaysSinceLockdown > -50) & (DaysSinceLockdown <76))


plot.movement.regime <- ggplot(data = movement.regime, mapping = aes(DaysSinceLockdown,mean.movement,group = regime)) +
  geom_line(aes(colour = regime)) +
  geom_vline(xintercept = 0) +
  theme_minimal()
plot.movement.regime

dfl <- arrange(dfl,Country,date.x)
plot.movement <- ggplot(data = dfl, mapping = aes(date.x,Movement,group = Country)) +
  geom_line(colour = "grey91") +
  theme(legend.position = "none") +
  theme_minimal()
plot.movement
