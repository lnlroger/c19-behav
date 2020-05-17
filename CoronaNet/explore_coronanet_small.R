

#source: https://coronanet-project.org/index.html

dta <- read.csv("CoronaNet/coronanet_release.csv") %>%
  mutate(continent = countrycode(ISO_A3, "iso3c", "continent")) %>%
  mutate(region = countrycode(ISO_A3, "iso3c", "region"))



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



