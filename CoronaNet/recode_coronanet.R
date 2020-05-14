###################
# The aim of this code is to reshape the coronanet data into a 
# format that lends itself to formal analysis at the country-day
# or country-week level.
# 
# Crucially, this means recoding events (which each correspond to 
# 1 observation in original data) into seperate variables.
# 
# The code's aim is to offer a flexible way of recoding events.
###################

rm(list = ls())
library("vars")
library("tidyverse")
library("pracma")
library("countrycode")


# Import raw data, which comes in Country-Day-Event format

dta <- read.csv("coronanet_release.csv") %>%
  mutate(continent = countrycode(ISO_A3, "iso3c", "continent", warn = FALSE)) %>%
  mutate(region = countrycode(ISO_A3, "iso3c", "region", warn = FALSE)) %>%
  mutate(compliance_binary = ifelse(substr(compliance,1,9) == "Mandatory", "Mandatory", # Binary variable whether lockdown is mandatory or voluntary
                                    ifelse(substr(compliance,1,9) == "Voluntary", "Voluntary", NA)))

# Before collapsing into country-day format, I want to extract dates of relevant events.
# "Relevant" should remain flexible, I want to be able to change the criteria quickly.

mandatory.national.lockdowns <- dta %>%
  mutate(mandatory.national.lockdown = 
           type == 'Quarantine/Lockdown' &
           init_country_level %in% c("National", "No, it is at the national level") &
           compliance_binary == "Mandatory") %>%
  dplyr::select(record_id,country,date_start,mandatory.national.lockdown) %>%
  distinct()

voluntary.national.lockdowns <- dta %>%
  mutate(voluntary.national.lockdown = 
           type == 'Quarantine/Lockdown' &
           init_country_level %in% c("National", "No, it is at the national level") &
           compliance_binary == "Voluntary") %>%
  dplyr::select(record_id,country,date_start,voluntary.national.lockdown) %>%
  distinct()

mandatory.national.curfews <- dta %>%
  mutate(mandatory.national.curfew = 
           type == 'Curfew' &
           init_country_level %in% c("National", "No, it is at the national level") &
           compliance_binary == "Mandatory") %>%
  dplyr::select(record_id,country,date_start,mandatory.national.curfew) %>%
  distinct()

# Merge into dta, creating dummy variable

tables.to.merge <- list(dta,
                        mandatory.national.lockdowns,
                        voluntary.national.lockdowns,
                        mandatory.national.curfews)


dta <- Reduce(function(...) merge(..., 
                                  by= c("record_id","country","date_start"), 
                                  all.x=TRUE),
              tables.to.merge) %>%
  distinct()

rm(mandatory.national.lockdowns,voluntary.national.lockdowns,mandatory.national.curfews,tables.to.merge)


# Plot those events against mobility (or whatever is of interest) ---

countries.plot <- c("United Kingdom", "France", "Italy")

# Prepare data as daily series of index and mobility by country, 
# as well as relevant lockdown/curfew dates.
dta.daily <-
  dta %>%
  group_by(country, date_start) %>%
  dplyr::summarise(
    index_high = mean(index_high_est, na.rm = TRUE),
    index_median = mean(index_med_est, na.rm = TRUE),
    index_low = mean(index_low_est, na.rm = TRUE),
    continent = first(continent),
    region = first(region),
    mandatory.national.lockdown = first(mandatory.national.lockdown),
    voluntary.national.lockdown = first(voluntary.national.lockdown),
    mandatory.national.curfew = first(mandatory.national.curfew)
  ) %>%
  arrange(country, date_start) %>%
  filter(as.Date(date_start) < as.Date("2020-04-20")) %>%
  left_join(read.csv("../df_covid_long.csv"),
            by = c("country" = "Country", "date_start" = "Date")) %>%
  dplyr::select(
    country,
    date_start,
    index_median,
    Movement,
    DateLockDown,
    total_cases,
    total_deaths,
    mandatory.national.lockdown,
    voluntary.national.lockdown,
    mandatory.national.curfew
  )
  
dta.plot.movement.index <-
  dta.daily %>%
  filter(country %in% countries.plot) %>%
  pivot_longer(
    -c(
      country,
      date_start,
      DateLockDown,
      mandatory.national.lockdown,
      voluntary.national.lockdown,
      mandatory.national.curfew
    ),
    names_to = "series",
    values_to = "value"
  ) %>%
  arrange(country, series, date_start)

# Plot
ggplot(dta.plot.movement.index,
       aes(
         x = as.Date(date_start),
         y = value,
         colour = series
       )) +
  geom_line(size = 1) +
  geom_vline(
    data = ddply(
      dta.plot.movement.index,
      "country",
      summarize,
      lockdown = as.Date(first(DateLockDown), format = "%d/%m/%Y")
    ),
    aes(xintercept = lockdown)
  ) +
  geom_segment(
    aes(
      y = -Inf,
      yend = Inf,
      x = as.Date(date_start),
      xend = as.Date(date_start),
      alpha = as.numeric(voluntary.national.lockdown)
    ),
    inherit.aes = F,
    colour = "green",
    size = 2
  ) +
  scale_alpha_continuous(range = c(0, 0.3)) +
  geom_segment(
    aes(
      y = -Inf,
      yend = Inf,
      x = as.Date(date_start),
      xend = as.Date(date_start),
      alpha = as.numeric(mandatory.national.lockdown)
    ),
    inherit.aes = F,
    colour = "red",
    size = 2
  ) +
  facet_grid(series ~ country, scales = "free") +
  theme(legend.position = "none")

rm(dta.plot.movement.index)

# VAR ----

country.var <- "France"

# Datasets for endogenous and exogenous variables
dta.var <-
  dta.daily %>%
  ungroup() %>%
  filter(country == country.var) %>%
  na.omit()

endog <-
  dta.var %>%
  mutate(
    Movement.diff = c(NA,diff(Movement)),
    total_cases.diff = c(NA,diff(total_cases)),
  ) %>%
  dplyr::select(Movement.diff, total_cases.diff) %>%
  slice(-1)

exog <-
  dta.var %>%
  mutate(mandatory.national.lockdown = as.numeric(mandatory.national.lockdown)) %>%
  dplyr::select(mandatory.national.lockdown) %>%
  slice(-1)

basic <-
  VAR(
    y = endog,
    p = 3,
    type = c("const"),
    exogen = exog,
    season = 7
  )

basic.irf <-
  irf(
    basic,
    cumulative = TRUE,
    ortho = TRUE,
    n.ahead = 14
  )
