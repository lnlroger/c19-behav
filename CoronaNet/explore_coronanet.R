rm(list = ls())
library("plyr")
library("tidyverse")
library("countrycode")

#source: https://coronanet-project.org/index.html

dta <- read.csv("CoronaNet/coronanet_release.csv") %>%
  mutate(continent = countrycode(ISO_A3, "iso3c", "continent")) %>%
  mutate(region = countrycode(ISO_A3, "iso3c", "region"))


# Dataset contains 14k events, distribution is as follows:

# By type ----
ggplot(dta,mapping = aes(y = type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# By sub-type for most frequent types

is.health.resources <- which(dta$type == "Health Resources")

ggplot(dta[is.health.resources,],mapping = aes(y = type_sub_cat)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

is.external.border <- which(dta$type == "External Border Restrictions")

ggplot(dta[is.external.border,],mapping = aes(y = type_sub_cat)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

is.nonessential.business <- which(dta$type == "Restriction of Non-Essential Businesses")

ggplot(dta[is.nonessential.business,],mapping = aes(y = type_sub_cat)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

is.schools <- which(dta$type == "Closure of Schools")

ggplot(dta[is.schools,],mapping = aes(y = type_sub_cat)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

is.lockdown <- which(dta$type == "Quarantine/Lockdown")

ggplot(dta[is.lockdown,],mapping = aes(y = type_sub_cat)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Types of quarantine (compliance)

dta <- mutate(dta, compliance_binary = ifelse(substr(compliance,1,9) == "Mandatory", "Mandatory",
                                              ifelse(substr(compliance,1,9) == "Voluntary", "Voluntary", NA)))

ggplot(dta[is.lockdown,],mapping = aes(y = compliance_binary)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# By country (Top 40) ----

country.count <- as.data.frame(table(dta$country)) %>%
  arrange(desc(Freq))

countries.to.plot <-  country.count[1:40,1]

ggplot(dta[which(dta$country %in% countries.to.plot),],mapping = aes(y = country)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Distribution of lockdown announcements in time

lockdown.per.day <- dta[is.lockdown,] %>%
  group_by(date_start) %>%
  tally()
  
ggplot(lockdown.per.day,mapping = aes(x = as.Date(date_start),y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Distribution of lockdown announcements by country

lockdown.per.country <- dta[is.lockdown,] %>%
  group_by(country) %>%
  tally() %>%
  arrange(desc(n))

ggplot(lockdown.per.country[1:40,],mapping = aes(x = country,y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Administrative level of lockdowns

dta <- mutate(dta, level_binary = ifelse(init_country_level %in% c("National", "No, it is at the national level"), 
                                         "National",
                                         ifelse(init_country_level %in% c("Municipal", "Yes, it is at the province/state level"),
                                                "Sub-National",
                                                NA
                                                )))

ggplot(dta[is.lockdown,], mapping = aes(y = level_binary)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Index ----

index.daily <- dta %>% # Daily series of index by country
  group_by(country, date_start) %>%
  summarise(index_high = mean(index_high_est, na.rm = TRUE),
            index_median = mean(index_med_est, na.rm = TRUE),
            index_low = mean(index_low_est, na.rm = TRUE),
            continent = first(continent),
            region = first(region)) %>%
  arrange(country, date_start) %>%
  filter(as.Date(date_start) < as.Date("2020-04-20")) 

index.daily.continent <- index.daily %>%
  group_by(continent, date_start) %>%
  summarise(index_high = median(index_high, na.rm = TRUE),
            index_median = median(index_median, na.rm = TRUE),
            index_low = median(index_low), na.rm = TRUE) %>%
  na.omit()

index.daily.region <- index.daily %>%
  group_by(region, date_start) %>%
  summarise(index_high = median(index_high, na.rm = TRUE),
            index_median = median(index_median, na.rm = TRUE),
            index_low = median(index_low), na.rm = TRUE) %>%
  na.omit()

index.daily.global <- index.daily %>%
  group_by(date_start) %>%
  summarise(index_high = median(index_high, na.rm = TRUE),
            index_median = median(index_median, na.rm = TRUE),
            index_low = median(index_low), na.rm = TRUE) %>%
  mutate(country = "World",
         continent = "World",
         region = "World") %>%
  na.omit()



# Index by country

ggplot(index.daily, mapping = aes(x = as.Date(date_start), y = index_median, colour = country)) +
  geom_line() +
  geom_smooth(data = index.daily.global, span = 0.2, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")

# Index by continent

ggplot(index.daily.continent, mapping = aes(x = as.Date(date_start), y = index_median, colour = continent)) +
  geom_smooth(span = 0.2, se=FALSE) +
  geom_smooth(data = index.daily.global, span = 0.2, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Index by region

ggplot(index.daily.region, mapping = aes(x = as.Date(date_start), y = index_median, colour = region)) +
  geom_smooth(span = 0.2, se=FALSE) +
  geom_smooth(data = index.daily.global, span = 0.2, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


rm(country.count,index.daily,index.daily.continent,index.daily.global,
   index.daily.region,lockdown.per.country,lockdown.per.day)

# Identify 'initial' mandatory lockdown and compare with dates we previously registered ----

first.lockdown <- dta %>%
  filter(type == "Quarantine/Lockdown",
         compliance_binary == "Mandatory",
         level_binary == "National") %>%
  group_by(country) %>%
  arrange(as.Date(date_start)) %>%
  mutate(first_lockdown = as.Date(date_start)) %>%
  select(country,first_lockdown) %>%
  slice(1) %>%
  distinct()


compare.lockdowns <- read.csv("../df_covid_short.csv") %>%
  select("Country", "DateLockDown") %>%
  mutate(country = Country) %>%
  select(-Country) %>%
  left_join(first.lockdown, "country") %>%
  na.omit()

ggplot(compare.lockdowns, aes(x = DateLockDown, y = first_lockdown)) +
  geom_point() +
  geom_smooth()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")


dta <- left_join(dta,first.lockdown, by = "country")

rm(compare.lockdowns, first.lockdown)

# Plot policy index versus mobility

countries.plot <- c("United Kingdom", "France", "Germany", "Italy")

dta.plot.movement.index <- dta %>% # Daily series of index and mobility by country
  group_by(country, date_start) %>%
  dplyr::summarise(index_high = mean(index_high_est, na.rm = TRUE),
            index_median = mean(index_med_est, na.rm = TRUE),
            index_low = mean(index_low_est, na.rm = TRUE),
            continent = first(continent),
            region = first(region)) %>%
  arrange(country, date_start) %>%
  filter(as.Date(date_start) < as.Date("2020-04-20")) %>%
  left_join(read.csv("../df_covid_long.csv"), by = c("country" = "Country", "date_start" = "Date")) %>%
  select(country, date_start, index_median, Movement, DateLockDown) %>%
  filter(country %in% countries.plot) %>%
  pivot_longer(-c(country,date_start,DateLockDown), names_to = "series", values_to = "value") %>%
  arrange(country, series, date_start)
  
  


ggplot(dta.plot.movement.index, aes(x = as.Date(date_start), y = value, colour = series)) +
  geom_line(size = 1) +
  geom_vline(data = ddply(dta.plot.movement.index, "country", summarize, lockdown = as.Date(first(DateLockDown), format = "%d/%m/%Y")), aes(xintercept=lockdown)) +
  facet_grid(series~country, scales = "free") +
  theme(legend.position = "none")


