UNpop <- read.csv("UN-Population/population_division_UN_Houseshold_Size_and_Composition_2019.csv") %>%
  rename(Country = �..Country) %>%
  mutate(Country = recode(Country, "Réunion" = "Reunion", "Saint-Barthélemy" = "Saint-Barthelemy")) %>%
  mutate(Country = countrycode(Country,"country.name","country.name")) %>%
  group_by(Country) %>%
  filter(date==last(date)) %>%
  distinct(Country,date)