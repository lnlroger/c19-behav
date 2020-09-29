#weather<-read.csv("Weather_new/WeatherJan-Jun-2020.csv")
library(tidyverse)
library(lubridate)
library(countrycode)

weather<-readRDS("weather.rds")%>%
  filter(!is.na(LATITUDE))%>%
  mutate(Lat_min_1=LATITUDE-0.1,
          Lat_max_1=LATITUDE+0.1,
          Lon_min_1=LONGITUDE-0.1,
          Lon_max_1=LONGITUDE+0.1)%>%
  mutate(Date=ymd(DATE))%>%
  mutate(Temp_C=(TEMP-32)*5/9)%>%
  mutate(DEWP=na_if(DEWP,9999.9))%>%
  mutate(SLP=na_if(SLP,9999.9))%>%
  mutate(STP=na_if(STP,999.9))%>%
  mutate(VISIB=na_if(VISIB,999.9))%>%
  mutate(WDSP=na_if(WDSP,999.9))%>%
  mutate(GUST=na_if(GUST,999.9))%>%
  mutate(MAX=na_if(MAX,9999.90))%>%
  mutate(MIN=na_if(MIN,9999.90))%>%
  mutate(PRCP=na_if(PRCP,99.990))%>%
  mutate(PRCP_Cm=1.54*PRCP)%>%
  mutate(SNDP=na_if(SNDP,999.9))%>%
  mutate(Dewp_C=(DEWP-32)*5/9)%>%
  mutate(Fog=as.numeric(substr(FRSHTT, 1, 1)))%>%
  mutate(Rain=as.numeric(substr(FRSHTT, 2, 2)))%>%
  mutate(Snow=as.numeric(substr(FRSHTT, 3, 3)))%>%
  mutate(Hail=as.numeric(substr(FRSHTT, 4, 4)))%>%
  mutate(Thunder=as.numeric(substr(FRSHTT, 5, 5)))%>%
  mutate(Tornado=as.numeric(substr(FRSHTT, 6, 6)))%>%
  mutate(CountryCode=str_sub(NAME,-2))%>%
  #mutate(Country=countrycode(CountryCode,origin='iso2c',destination='country.name'))
  mutate(Country=countrycode(CountryCode,origin='fips',destination='country.name'))




  
#Not to be trusted with country names - there are several non-matches. Surprising that IC, EZ, etc. are not matched unambiguously...




#  naniar::replace_with_na_all(condition = ~.x==999.9) #wow... this takes for ever and, not all NAs are in 999.9 form






weather_short<-weather%>%
  group_by(LATITUDE,LONGITUDE)%>%
  summarise(Country=first(Country),Elevation=mean(ELEVATION),Station=first(STATION),StationName=first(NAME),Temp=mean(TEMP),
            AvgDewPoint=mean(DEWP),AvgSLPressure=mean(SLP),AvgVisib=mean(VISIB),AvgWndSpeed=mean(WDSP),AvgMxWndSpeed=mean(MXSPD),
            AvgGust=mean(GUST),AvgPrcp=mean(PRCP),
            AvgSnowDep=mean(SNDP),AvgFog=mean(Fog),AvgRain=mean(Rain),AvgSnow=mean(Snow),AvgHail=mean(Hail),AvgThunder=mean(Thunder),AvgTornado=mean(Tornado))






################Terms
#Read the Readme file

###DEWP: (mean of the day) dew point: the temperature to which air must be cooled to become saturated with water vapor. 
#When cooled further, the airborne water vapor will condenxe to form liquid water (dew). 
#The higher the dew point, the more water vapor there is in the air. 

###SLP: (mean of the day) sea level pressure. Measured in mb. Average sea-level pressure is 1013.25 mbar (101.325 kPa; 29.921 inHg; 760.00 mmHg).
#High SLP in Siberia where the pressure goes above 1050mb
#Low SLP: at the centre of tropical cyclones and tornadoes with a record low of 870mb

###STP: (mean of the day) station pressure

###VISIB: visibility

###WDSP: wind speed

###MXSPD: Maximum Sustained Wind Speed

###GUST: A wind "gust" is also reported when the peak "instantaneous" wind during the most recent ten-minutes prior to the observation is more than 
#10 knots greater than the lowest "lull" in the wind during that time. 
#If that is the case, the highest instantaneous wind during that ten minute window is reported as the gust value. 
#The wind speed is recorded by a pen mark on piece of paper held on a rotating drum, attached to the anemometer output. 
#Hence the gusts is the spike in the chart that rolls out with tme.

###MAX: I presume this refers to the max gust


### PRCP: Percipitation - this is not a mean but an amount over the day, measured in .01 inches. 

### SNDP: Snow depth, measured in .1 inches

# FRSHTT: Indicator occurence of: Fog, Rain or Drizzle, Snow or Ice Pellets, Hail, Thunder, Tornado/Funnel Cloud. 
# So a 10000 score means that there was Fog and nothing else. 

