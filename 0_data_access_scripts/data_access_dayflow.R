#get teh dayflow data

library(tidyverse)
library(lubridate)

#I need to read in the Dayflow data from the CNRA portal
# https://data.cnra.ca.gov/dataset/dayflow
#Still needs a little fiddling, but much better!.

Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")

DF1929_1939 = Dayflow$`Dayflow Results 1929 - 1939`%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT1) %>%
select(Date, OUT)
  
DF1940_1949 = Dayflow$`Dayflow Results 1940 - 1949` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT2) %>%
  select(Date, OUT)

DF1950_1955= Dayflow$`Dayflow Results 1950 - 1955` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT2) %>%
  select(Date, OUT)

DF1956_1969 = Dayflow$`Dayflow Results 1956 - 1969` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

DF1970_1983 = Dayflow$`Dayflow Results 1970 - 1983` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

DF1984_1996 = Dayflow$`Dayflow Results 1984 - 1996` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

DF1997_2019 =  Dayflow$`Dayflow Results 1997 - 2019` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT)

#now I can put them all together!
DF = bind_rows(DF1929_1939, DF1940_1949, DF1950_1955, DF1956_1969, DF1970_1983, DF1984_1996, DF1997_2019)

write.csv(DF, file.path(data_root,"dayflow_all.csv"), row.names = FALSE)
