library("tidyverse")

apple <- read.csv("applemobilitytrends-2020-09-10.csv") %>%
  select(-alternative_name) %>%
  pivot_longer(cols = starts_with("X"), names_to = "Date", names_prefix = "X", 
               values_to = "Movement") %>%
  filter(geo_type == "country/region") %>%
  mutate(Country = as.character(region),
         Date = as.Date.character(Date,format = "%d.%m.%Y"),
         Movement.apple = Movement)

long <- readRDS("../df_covid_long.rds") %>%
  filter(sub_region_1 == "") %>%
  mutate(Country = as.character(Country),
         Movement.google = Movement)

together <- left_join(apple,long,by = c("Country","Date")) %>%
  select(Country, Date, transportation_type, Movement.apple, 
         Movement.google,retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline,
         workplaces_percent_change_from_baseline,
         residential_percent_change_from_baseline
         ) %>%
  na.omit()


## Walking ----

# Movement aggregated
regit <- together %>%
  filter(transportation_type == "walking") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ Movement.google, data = .x))) %>%
  filter(term == "Movement.google")

summary(regit$estimate)


# retail_and_recreation_percent_change_from_baseline

regit <- together %>%
  filter(transportation_type == "walking") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ retail_and_recreation_percent_change_from_baseline, data = .x))) %>%
  filter(term == "retail_and_recreation_percent_change_from_baseline")

summary(regit$estimate)


# grocery_and_pharmacy_percent_change_from_baseline

regit <- together %>%
  filter(transportation_type == "walking") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ grocery_and_pharmacy_percent_change_from_baseline, data = .x))) %>%
  filter(term == "grocery_and_pharmacy_percent_change_from_baseline")

summary(regit$estimate)

# parks_percent_change_from_baseline     

regit <- together %>%
  filter(transportation_type == "walking") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ parks_percent_change_from_baseline, data = .x))) %>%
  filter(term == "parks_percent_change_from_baseline")

summary(regit$estimate)

# transit_stations_percent_change_from_baseline
regit <- together %>%
  filter(transportation_type == "walking") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ transit_stations_percent_change_from_baseline, data = .x))) %>%
  filter(term == "transit_stations_percent_change_from_baseline")

summary(regit$estimate)


# workplaces_percent_change_from_baseline          

regit <- together %>%
  filter(transportation_type == "walking") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ workplaces_percent_change_from_baseline, data = .x))) %>%
  filter(term == "workplaces_percent_change_from_baseline")

summary(regit$estimate)


# residential_percent_change_from_baseline

regit <- together %>%
  filter(transportation_type == "walking") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ residential_percent_change_from_baseline, data = .x))) %>%
  filter(term == "residential_percent_change_from_baseline")

summary(regit$estimate)

## Driving ----

# Movement aggregated
regit <- together %>%
  filter(transportation_type == "driving") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ Movement.google, data = .x))) %>%
  filter(term == "Movement.google")

summary(regit$estimate)


# retail_and_recreation_percent_change_from_baseline

regit <- together %>%
  filter(transportation_type == "driving") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ retail_and_recreation_percent_change_from_baseline, data = .x))) %>%
  filter(term == "retail_and_recreation_percent_change_from_baseline")

summary(regit$estimate)


# grocery_and_pharmacy_percent_change_from_baseline

regit <- together %>%
  filter(transportation_type == "driving") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ grocery_and_pharmacy_percent_change_from_baseline, data = .x))) %>%
  filter(term == "grocery_and_pharmacy_percent_change_from_baseline")

summary(regit$estimate)

# parks_percent_change_from_baseline     

regit <- together %>%
  filter(transportation_type == "driving") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ parks_percent_change_from_baseline, data = .x))) %>%
  filter(term == "parks_percent_change_from_baseline")

summary(regit$estimate)

# transit_stations_percent_change_from_baseline
regit <- together %>%
  filter(transportation_type == "driving") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ transit_stations_percent_change_from_baseline, data = .x))) %>%
  filter(term == "transit_stations_percent_change_from_baseline")

summary(regit$estimate)


# workplaces_percent_change_from_baseline          

regit <- together %>%
  filter(transportation_type == "driving") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ workplaces_percent_change_from_baseline, data = .x))) %>%
  filter(term == "workplaces_percent_change_from_baseline")

summary(regit$estimate)


# residential_percent_change_from_baseline

regit <- together %>%
  filter(transportation_type == "driving") %>%
  group_by(Country) %>%
  group_modify(~ broom::tidy(lm(Movement.apple ~ residential_percent_change_from_baseline, data = .x))) %>%
  filter(term == "residential_percent_change_from_baseline")

summary(regit$estimate)
