register_google(key = "AIzaSyAHf0RUy4W9WiRQDzWG3jVV8UwLUxcmHyY")
map1 <- get_map(location = "Magallanes", zoom = 6)

uk <- GpsGoog %>%
  filter(Country.x == "United Kingdom")
uk.gps_coord <- gps_coord %>%
  filter(Country == "United Kingdom")

charg <- GpsGoog 
  filter(Country.x == "Chile" | Country.x == "Argentina")
charg.gps_coord <- gps_coord 
  filter(Country == "Chile" | Country == "Argentina")

ggmap(ggmap = map1) +  
  geom_rect(data=charg, mapping=aes(xmin=West, xmax=East, ymin=South, ymax=North), color="red", alpha=0.5) +
  geom_point(data = charg.gps_coord, aes(x=lon,y=lat), colour = "blue")
