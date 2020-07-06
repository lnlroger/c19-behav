#Source: https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-day?pageSize=100&dataTypes=%5B%22DEWP%22,%22FRSHTT%22,%22GUST%22,%22MAX%22,%22MIN%22,%22MXSPD%22,%22PRCP%22,%22SLP%22,%22SNDP%22,%22STP%22,%22TEMP%22,%22VISIB%22,%22WDSP%22%5D&bbox=%5B%2239.266,22.500,36.880,25.357%22%5D&startDate=%5B%222019-12-01T00:00:00%22%5D&endDate=%5B%222020-01-01T23:59:59%22%5D


file.list <- list.files(pattern='*csv')
df.list <- lapply(file.list, read.csv)
d<-do.call("rbind", df.list)

setwd("C:/Users/fores/OneDrive - The University of Nottingham/GitHub/COVID-19")
source("ImportLong.R")

q<-df%>%
  group_by(Country)%>%
  filter(!is.na(Movement),!is.na(StringencyIndex),!is.na(risktaking))%>%
  summarise(m=mean(Movement))
q$Country
