rm(list = ls())
source("ARDL_fun.R")

# Prepare dataset (take differences, drop unnecessary variables and missing observations)
df.now <- read_rds("../df_covid_long.rds") %>%
  filter(sub_region_1 == "") %>%
  group_by(Country) %>%
  mutate(diff.Movement = c(NA, diff(Movement)),
         diff.StringencyIndex = c(NA, diff(StringencyIndex))) %>%
  dplyr::select(Country, Date, diff.Movement, diff.StringencyIndex) %>%
  drop_na()

# Run function
test.ARDL <- ARDL.flex()
