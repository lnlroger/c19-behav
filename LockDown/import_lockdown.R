#lockdown has been customised so as to take the median Province's attitude. 
#The median is calculated by arranging by date of lockdown.
#This approach coerces the US to the lockdown from its median state: N. Hampshire for example.
#If you want to see the lock-down dates per state, see the "CountryLockdowndates.csv" instead.
lockdown<- read.csv("LockDown/countryLockdowndates_custom.csv") %>%
  rename(DateLockDown=Date)%>%
  mutate(Lock=ifelse(Type=="None","No","Yes"))%>%
  mutate(Country=countrycode(Country,'country.name','country.name'))
