#get teh dayflow data

library(tidyverse)
library(lubridate)

#I need to read in the Dayflow data from the CNRA portal
# https://data.cnra.ca.gov/dataset/dayflow
#Still needs a little fiddling, but much better!.

DF1929_1939 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/ab12e85f-82f4-4723-9973-deeed41b2057/download/dayflow-results-1929-1939.csv",
                       stringsAsFactors = F, row.names = NULL)%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT1) %>%
select(Date, OUT)
  
DF1940_1949 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/bf58c67c-63b4-47d4-9a25-2b95e5479a0c/download/dayflow-results-1940-1949.csv",
                       stringsAsFactors = F, row.names = NULL)%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT2) %>%
  select(Date, OUT)

DF1950_1955= read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/9225dbe7-54a6-4466-b360-e66f51407683/download/dayflow-results-1950-1955.csv",
                      stringsAsFactors = F, row.names = NULL)%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT2) %>%
  select(Date, OUT)

DF1956_1969 = read.csv('https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/3109f3ef-b77b-4288-9ece-3483899d10da/download/dayflow-results-1956-1969.csv',
stringsAsFactors = F, row.names = NULL)%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

DF1970_1983 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/a0a46a1d-bec5-4db9-b331-655e306860ba/download/dayflow-results-1970-1983.csv",
                       row.names = NULL, stringsAsFactors = F)%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

DF1984_1996 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/dayflow-results-1984-1996.csv",
                       row.names = NULL, stringsAsFactors = F)%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

DF1997_2019 = read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2019.csv",
                       row.names = NULL, stringsAsFactors = F)%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

#now I can put them all together!
DayFlow = bind_rows(DF1929_1939, DF1940_1949, DF1950_1955, DF1956_1969, DF1970_1983, DF1984_1996, DF1997_2019)

write.csv(DayFlow, file.path(data_root,"dayflow_all.csv"))
