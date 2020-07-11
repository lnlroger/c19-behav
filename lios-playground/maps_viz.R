register_google(key = "AIzaSyAHf0RUy4W9WiRQDzWG3jVV8UwLUxcmHyY")
map1 <- get_map(location = "United Kingdom", zoom = 5)

uk <- GpsGoog %>%
  filter(Country.x == "United Kingdom")
uk.gps_coord <- gps_coord %>%
  filter(Country == "United Kingdom")

ggmap(ggmap = map1) +
  geom_rect(data=uk, mapping=aes(xmin=west, xmax=east, ymin=south, ymax=north), color="green", alpha=0.5) +
  geom_rect(data=uk, mapping=aes(xmin=West, xmax=East, ymin=South, ymax=North), color="red", alpha=0.5) +
  geom_point(data = uk.gps_coord, aes(x=lon,y=lat), colour = "blue")
