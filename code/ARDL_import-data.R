rm(list = ls())
library("tidyverse")
library("lubridate")
library("vars")
library("zoo")

# Load data ----

# Import
df <- read_rds("../df_covid_long.rds") %>%
  filter(!is.na(StringencyIndex)) %>%
  mutate(Date = ymd(Date))

OxVars<-c('C1_School.closing', 'C1_Flag', 'C2_Workplace.closing','C2_Flag','C3_Cancel.public.events','C3_Flag','C4_Restrictions.on.gatherings','C4_Flag','C5_Close.public.transport','C6_Stay.at.home.requirements','C6_Flag','C7_Restrictions.on.internal.movement','C7_Flag,C8_International.travel.controls','H1_Public.information.campaigns','H1_Flag')

GoogleVars<-c('retail_and_recreation_percent_change_from_baseline', 'grocery_and_pharmacy_percent_change_from_baseline', 'parks_percent_change_from_baseline', 'transit_stations_percent_change_from_baseline', 'workplaces_percent_change_from_baseline','residential_percent_change_from_baseline','Movement')

# Merge stuff and rearrange

CoronaNet <- read.csv(file = "../CoronaNet/CononaNetDaily.csv") %>%
  mutate(Country = country,
         Date = ymd(date_start),
         IndexCoronaNet = index_median) 

  
df<- df %>%
  left_join(CoronaNet[,c("Country","Date","IndexCoronaNet")],
            by = c("Country","Date"),
            ) %>%
  group_by(Country) %>%
  mutate(IndexCoronaNet = na.locf(
    IndexCoronaNet,
    na.rm = FALSE))

rm(GoogleVars, OxVars, CoronaNet)

# Select variables of interest
df.use <- df %>%
  dplyr::select(Country, Date, week,
         total_cases, new_cases,
         total_deaths, new_deaths,
         Google, 
         C1_School.closing, C1_Flag,
         C2_Workplace.closing, C2_Flag,
         C3_Cancel.public.events, C3_Flag,
         C4_Restrictions.on.gatherings, C4_Flag,
         C5_Close.public.transport, C5_Flag,
         C6_Stay.at.home.requirements, C6_Flag,
         C7_Restrictions.on.internal.movement, C7_Flag,
         C8_International.travel.controls,
         StringencyIndex, StringencyIndexForDisplay, 
         IndexCoronaNet,
         Movement, 
         polity2, risktaking, patience, ROL,
         GDP.capita, CaseLog,
         Continent, Region) %>%
  mutate(Movement_lag_1d=dplyr::lag(Movement,1))%>%
  mutate(Movement_lead_1d=dplyr::lead(Movement,1))%>%
  mutate(DifMove=Movement-Movement_lag_1d)%>%
  mutate(DifMoveLag=dplyr::lag(DifMove,1))%>%
  mutate(DifPol=IndexCoronaNet-dplyr::lag(IndexCoronaNet,1))%>%
  ungroup()

rm(df)


