rm(list = ls())
source("ARDL_fun.r")
library("tidyverse")
df <- readRDS("../CityLevel_DoARDL.rds")

df2<- df %>% 
  mutate(diff.Movement = c(NA, diff(Movement)),
         diff.StringencyIndex = c(NA, diff(StringencyIndex)),
         diff.PRCP = c(NA, diff(PRCP)),
         diff.Temp_C = c(NA, diff(Temp_C))) %>%
  dplyr::select(Country,City, Date, diff.Movement, diff.StringencyIndex,
                diff.PRCP, diff.Temp_C) %>%
  drop_na()

df2$diff.PRCP <- df2$diff.PRCP +rnorm(length(df2$diff.PRCP),0.01)

ARDL_cov <- ARDL.flex(data.input = df2,depvar = "diff.Movement", indepvar = c("diff.StringencyIndex",
                                                                  "diff.PRCP",
                                                                  "diff.Temp_C"),by.var = "City",
                      max.p = 7, max.q = 7,
                      t.min = 50)

