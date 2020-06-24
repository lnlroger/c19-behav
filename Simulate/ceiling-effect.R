rm(list = ls())
library("dLagM")
library("tidyverse")

df <- read.csv("mock-series.csv")
colnames(df)[1] <- "Series"

# ARDL ----
df.ARDL <- df %>%
  group_by(Series) %>%
  mutate(Movement = Movement + rnorm(length(Movement),0,0.01),
         Stringency = Stringency + rnorm(length(Stringency),0,0.01),) %>%
  mutate(diff.Movement = c(NA, diff(Movement)),
         diff.Stringency = c(NA, diff(Stringency)))  %>%
  drop_na()


# Gradual moderate

df.gradual_moderate <- df.ARDL %>%
  filter(Series == "immediate_nonlinear")

gradual_moderate.ARDL <- ardlDlm(formula = diff.Movement ~ diff.Stringency, 
                      data = df.gradual_moderate,
                      p = 3 , q = 3)

gradual_moderate.ARDL$model$coefficients

gradual_moderate.LR <- sum(gradual_moderate.ARDL$model$coefficients[c("diff.Stringency.t",paste0("diff.Stringency.",1:3))]) /
  (1-sum(gradual_moderate.ARDL$model$coefficients[paste0("diff.Movement.",1:3)]))

gradual_moderate.LM <- lm(diff.Movement ~ diff.Stringency, data = df.gradual_moderate)

gradual_moderate.LR
cor(df.gradual_moderate$Stringency,
    df.gradual_moderate$Movement)
gradual_moderate.LM$coefficients

# Gradual moderate


