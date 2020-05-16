#Source: there are multiple sources all detailed in the .csv file. 
#The excel file was found here: https://www.kaggle.com/jcyzag/covid19-lockdown-dates-by-country
#About this data: The data was acquired by going through each country that had at least 1 confirmed case. Searching for news articles, 
  #wikipedia and government websites to identify when the lockdown was started. 
  #A lockdown is assumed when schools/universities and any non-essential businesses are closed.

#lockdown has been customised so as to take the median Province's attitude. 
#The median is calculated by arranging by date of lockdown.
#This approach coerces the US to the lockdown from its median state: N. Hampshire for example.
#If you want to see the lock-down dates per state, see the "CountryLockdowndates.csv" instead.
lockdown<- read.csv("LockDown/countryLockdowndates_custom.csv") %>%
  rename(DateLockDown=Date)%>%
  mutate(Lock=ifelse(Type=="None","No","Yes"))%>%
  mutate(Country=countrycode(Country,'country.name','country.name'))
