library("tidyverse")
library("countrycode")

`%notin%` <- Negate(`%in%`) # Defines logical operator "not in" for use below

# Import google mobility data ----

source("Google/import_mobility.R")

# Lockdown dates ----

source("LockDown/import_lockdown.R")

# Cases and Deaths ----

source("CasesDeaths/import_cases_deaths.R")

# Weather data ----

source("Weather/import_weather.R")

# World Value Survey ----

source("WVS/import_wvs.R")

# World Bank data (Rule of Law, Communicable diseases, hospital beds among others) ----

source("WB/import_wb.R")

# PolityIV index ----

source("Politics/import_polityIV.R")

# UN population data ----

source("UN-Population/import_unpop.R")

# Elections ----
  
source("Politics/import_dpi.R")
  
# Social preferences ----

source("Briq/import_social-prefs.R")

# Basic country co-variates (source?) ----

source("Countries/import_covariates.R")

# Collectivism - Hofstede ----

source("Collectivism/import_collectivism.R")


# Gelfand data (Government efficiency) ----

source("Government_Gelfand/import_gelfand.R")  

# Previous epidemics ----

source("EM-DAT/import_epidemics.R")

# Merge into single dataframes ----

# Short version (pure cross-section)

df_short <- merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(
  mobility_short,
  DaysLock_short,by="Country",all=T),
  wvs,by="Country",all=T),
  weather_short,by="Country",all=T),
  wb,by="Country",all=T),
  elections,by="Country",all=T),
  rol,by="Country",all=T),
  social_prefs,by="Country",all=T),
  countries,by="Country",all=T),
  polityIV, by="Country",all=T),
  UNpop, by="Country",all=T),
  hf,by="Country",all=T),
  Gf_gov,by="Country",all=T)


df_short<-df_short%>%
  filter(!is.na(Country))

df_short$Death_pc<-df_short$TotalDeaths/df_short$Population
df_short$Confirmed_pc<-df_short$TotalCases/df_short$Population
df2<-subset(df_short,df_short$Province!="Faroe Islands")
df2$Log_Death_pc<-ifelse(df2$Death_pc>0,log(df2$Death_pc),NA)
df2$Google_pc<-df2$Google/df2$Population
df2$Log_Google_pc<-ifelse(df2$Google_pc>0,log(df2$Google_pc),NA)

df2<-df2%>%
  mutate(DateLockDown=as.Date(DateLockDown,format="%d/%m/%Y"))%>%
  mutate(Date=as.Date(Date))



write.csv(df2,"04052020_short.csv")


# Long version (daily data)

df_long<-merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(
  mobility_weather_death,
  lockdown,by="Country",all=T),
  wvs,by="Country",all=T),
  wb,by="Country",all=T),
  elections,by="Country",all=T),
  rol,by="Country",all=T),
  social_prefs,by="Country",all=T),
  countries,by="Country",all=T),
  time_short,by="Country",all=T),
  polityIV,by="Country",all=T),
  UNpop,by="Country",all=T),
  hf,by="Country",all=T),
  Gf_gov,by="Country",all=T)

df_long<-df_long%>%
  filter(!is.na(Country))


df_long$Death_pc<-df_long$total_deaths/df_long$Population
df_long$Confirmed_pc<-df_long$total_cases/df_long$Population
df3<-subset(df_long,df_long$Province!="Faroe Islands")
df3$Log_Death_pc<-ifelse(df3$Death_pc>0,log(df3$Death_pc),NA)

df3$Google_pc<-df3$DeathsBeforeGoogle/df3$Population
df3$Log_Google_pc<-ifelse(df3$Google_pc>0,log(df3$Google_pc),NA)
df3$DateLockDown <- df3$DateLockDown.y 


write.csv(df3,"04052020_long.csv")


