rm(list = ls())
source("ARDL_fun.R")
library("tidyverse")

# Most basic test ----

# Prepare dataset (take differences, drop unnecessary variables and missing observations)
df.now <- read_rds("../df_covid_long.rds") %>%
  filter(sub_region_1 == "") %>%
  group_by(Country) %>%
  mutate(diff.Movement = c(NA, diff(Movement)),
         diff.StringencyIndex = c(NA, diff(StringencyIndex))) %>%
  dplyr::select(Country, Date, diff.Movement, diff.StringencyIndex) %>%
  drop_na()

# Run function
test.ARDL <- ARDL.flex(max.p = 3, max.q = 3)

# Test with different variables ----

# Prepare dataset (take differences, drop unnecessary variables and missing observations)
df.now <- read_rds("../df_covid_long.rds") %>%
  filter(sub_region_1 == "") %>%
  group_by(Country) %>%
  mutate(dff.Movement = c(NA, diff(Movement)),
         diff.StringencyIn = c(NA, diff(StringencyIndex)),
         diff.ConfirmedCases =c(NA, diff(ConfirmedCases)), 
         ) %>%
  dplyr::select(Country, Date, dff.Movement, diff.StringencyIn, diff.ConfirmedCases) %>%
  drop_na()

# Run function
test.ARDL <- ARDL.flex(max.p = 2, max.q = 2, 
                       depvar = "dff.Movement", 
                       indepvar = c("diff.StringencyIn", "diff.ConfirmedCases"),
                       impulsevar = "diff.ConfirmedCases",vars.to.deseason = "dff.Movement")

# Test with different administrative level ----

df.now <- read_rds("../df_covid_long.rds") %>%
  filter(sub_region_1 != "") %>%
  group_by(sub_region_1) %>%
  mutate(diff.Movement = c(NA, diff(Movement)),
         diff.StringencyIndex = c(NA, diff(StringencyIndex))) %>%
  dplyr::select(sub_region_1, Date, diff.Movement, diff.StringencyIndex) %>%
  drop_na()

# Run function
test.ARDL <- ARDL.flex(max.p = 2, max.q = 2, by.var = "sub_region_1")



