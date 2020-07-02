#Files that require regular updating
## Case Death
## Google
## Oxford


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

# Import long run coefficients obtained from ARDL

lr.coeffs <- read_rds("compliance/LongRunCoefficients_ARDL.rds")

# Merge into single dataframes ----


# Short version (pure cross-section)


source("OxfordTracking/covid-policy-tracker-master/data/import_OxCGRT.R")

datasets.to.merge.short <- list(mobility_short,
                          DaysLock_short,
                          wvs,
                          weather_short,
                          wb,
                          elections,
                          rol,
                          social_prefs,
                          countries,
                          polityIV,
                          UNpop, 
                          hf,
                          Gf_gov,
                          lr.coeffs
                          )

df_short <- Reduce(function(...) full_join(..., by='Country'), datasets.to.merge.short) %>%
  filter(!is.na(Country)) %>%
  mutate(Death_pc = TotalDeaths/Population) %>%
  mutate(Death_pc = TotalDeaths/Population) %>%
  mutate(Confirmed_pc = TotalCases/Population) %>%
  filter(Province != "Faroe Islands") %>%
  mutate(Log_Death_pc = ifelse(Death_pc>0,log(Death_pc),NA)) %>%
  mutate(Google_pc = Google/Population) %>%
  mutate(Log_Google_pc = ifelse(Google_pc>0,log(Google_pc),NA)) %>%
  mutate(DateLockDown=as.Date(DateLockDown,format="%d/%m/%Y"))%>%
  mutate(Date=as.Date(Date))



write.csv(df_short,"df_covid_short.csv")


# Long version (daily data)

datasets.to.merge.long <- list(mobility_weather_death,
                               lockdown,
                               wvs,
                               wb,
                               elections,
                               rol,
                               social_prefs,
                               countries,
                               time_short,
                               polityIV,
                               UNpop,
                               hf,
                               Gf_gov,
                               lr.coeffs
                               )


df_long<- Reduce(function(...) full_join(..., by=c('Country')), datasets.to.merge.long) %>%
  filter(!is.na(Country)) %>%
  mutate(Death_pc = total_deaths/Population) %>%
  mutate(Confirmed_pc = total_cases/Population) %>%
  filter(Province != "Faroe Islands") %>%
  mutate(Log_Death_pc = ifelse(Death_pc>0,log(Death_pc),NA)) %>%
  mutate(Google_pc = DeathsBeforeGoogle/Population) %>%
  mutate(Log_Google_pc = ifelse(Google_pc>0,log(Google_pc),NA)) %>%
  mutate(DateLockDown = DateLockDown.y)

df_long<-merge(df_long,Ox,by=c("Country","Date"),all=T)


#write.csv(df_long,"df_covid_long.csv")
write_rds(df_long,"df_covid_long.rds")

