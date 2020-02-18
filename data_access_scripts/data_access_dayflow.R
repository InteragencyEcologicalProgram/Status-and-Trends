#get teh dayflow data

library(tidyverse)
library(lubridate)

#I need to read in the Dayflow data from the CNRA portal
# https://data.cnra.ca.gov/dataset/dayflow
#Unfortunately, there are all sorts of unconsistancies, and I can't figure out how to bind them all
#together without fiddling with each one first.

DF1929_1939 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/ab12e85f-82f4-4723-9973-deeed41b2057/download/oct1929-dec1939.csv",
                       stringsAsFactors = F, row.names = NULL)%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT1) %>%
select(Date, OUT)
  
DF1940_1949 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/bf58c67c-63b4-47d4-9a25-2b95e5479a0c/download/jan1940-dec1949.csv",
                       stringsAsFactors = F, row.names = NULL)%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT2) %>%
  select(Date, OUT)

DF1950_1955= read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/9225dbe7-54a6-4466-b360-e66f51407683/download/jan1950-sep1955.csv",
                      stringsAsFactors = F, row.names = NULL)%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT2) %>%
  select(Date, OUT)

DF1956_1969 = read.csv('https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/3109f3ef-b77b-4288-9ece-3483899d10da/download/wy1956-1969.csv',
stringsAsFactors = F, row.names = NULL)%>%
  mutate( Date = as.Date(Date, format = "%d-%b-%y")- period(year = 100)) %>%
  select(Date, OUT)

DF1970_1983 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/a0a46a1d-bec5-4db9-b331-655e306860ba/download/wy1970-1983.csv",
                       row.names = NULL, stringsAsFactors = F)%>%
  mutate( Date = as.Date(DATE, format = "%d-%b-%y")) %>%
  select(Date, OUT)

DF1984_1996 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/wy1984-1996.csv",
                       row.names = NULL, stringsAsFactors = F)%>%
  mutate( Date = as.Date(DATE, format = "%d-%b-%y")) %>%
  select(Date, OUT)

DF1997_2018 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2018.csv",
                       row.names = NULL, stringsAsFactors = F)%>%
  mutate( Date = as.Date(Date, format = "%d-%b-%y")) %>%
  select(Date, OUT)

#2019 was really formatted wierd.
DF2019 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/2050711b-4fd4-4279-9d19-8e3e24a2361b/download/dayflowcalculations2019.csv",
                 row.names = NULL, stringsAsFactors = F)%>%
  mutate( Date = as.Date(Mo, format = "%d%b%Y"), OUT = as.numeric(RIO)) %>%
  select(Date, OUT) %>%
  filter(!is.na(Date))

#now I can put them all together!
DayFlow = bind_rows(DF1929_1939, DF1940_1949, DF1950_1955, DF1956_1969, DF1970_1983, DF1984_1996, DF1997_2018, DF2019)

write.csv(DayFlow, "data/dayflow_all.csv")
