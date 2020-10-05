rm(list=ls())

library("tidyverse")

dta <- readRDS("../df_covid_long.rds") %>%
  filter(sub_region_1 == "")
  
coronanet <- read.csv(file = "../CoronaNet/coronanet_release.csv") %>%
  mutate(Date = as.Date(date_start),
         Country = as.character(country))

dta2 <- left_join(dta,coronanet, by = c("Country","Date")) %>%
  group_by(Country, Date) %>% 
  mutate(description.combined = paste0(event_description, collapse = ""),
         type.combined = paste0(type, collapse = "")) %>%
  select(Date,Country,StringencyIndex,index_med_est,description.combined,type.combined) %>%
  summarise_all(first) %>%
  fill(index_med_est, .direction = "down")

write_rds(dta2,"../oxford_coronanet.rds")
