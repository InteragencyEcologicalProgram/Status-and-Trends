## Data retrieval script for CDFW's 20mm Survey.
## EDI publication now has length data!

# Surveys 1 though 9 are considered for the purpose of index calculation. The index is calculated using the data
# from only 4 of these surveys: the two before and the two after the point where the average length of delta smelt
# (less than 60mm in length) equals 20mm. From this subset of surveys, the delta smelt catch-per-unit-effort
# (CPUE) is calculated for each of the 41 “core” stations. One (1) is added to each CPUE value and then a log10
# transformation is preformed. For example:
#   CPUE 3.45
# CPUE+1 4.45
# Log10Trans = log(4.45)/(log10) 0.65
# These calculations are made for each station within a given survey. The average is taken of all the resulting
# “Log10Trans” values within a survey in order to obtain one value. The geometric mean is calculated on this
# average value, like so:
#   10^(Avg([Log10 Trans]))-1
# The 20mm delta smelt index is the summation of the 4 geometric means

source("setup.R")
library(readxl)
###################################################################################################

thisDataRoot <- file.path(data_root,"20mm")

## 20mm Survey url:
#Joined long formatted data frame of the base tables. This table is produced from the LTMRdata package

twentymil = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.535.5&entityid=1e744fd85f10c4164d748831bc0454dd")



## Index stations based on CDFW memos, adding the new stations from teh north delta for 2022
index_stations <- data.frame("Station"=c(
  346,345,344,343,342,340,323,405,418,411,602,501,504,519,606,609,610,508,513,520,
  801,804,703,704,706,705,707,809,812,815,901,902,906,919,910,912,914,915,918,711,
  716, 718, 719, 720, 723, 724, 726)
)


#Matrix of CPUE w/zeros

twentyzeros = twentymil %>%
  mutate(Year = year(Date)) %>%
  filter(Station %in% index_stations$Station,
         Length <60 |is.na(Length)) %>%
  group_by(Year, Date, Station, TowNum, Tow_volume, Survey, Taxa) %>%
  summarize(Count = sum(Count)) %>%
  pivot_wider(id_cols = c(Year, Date, Station, TowNum, Tow_volume, Survey),
              names_from = Taxa, values_from = Count, values_fill = 0) %>%
  pivot_longer(cols = c(`NA`:last_col()), names_to = "Taxa", values_to = "Count")



##########################################################################
## Delta Smelt

AvgDSlength = twentymil %>%
  mutate(Year = year(Date)) %>%
  filter(Station %in% index_stations$Station,
         Taxa == "Hypomesus transpacificus", Length <60) %>%
  group_by(Year, Survey) %>%
  summarize(Length = mean(Length)) %>%
  group_by(Year) %>%
  mutate(lag1 = lag(Length),
         lag2 = lag(lag1),
         lead1 = lead(Length),
         lead2 = lead(lead1),
         UseforIndex = case_when((Length <20 & lead1 >20 )|(Length <20 &lead2 >20) ~ TRUE,
                                 (Length >20 & lag1 <20 )| (Length >20 &lag2 <20) ~ TRUE,
                                 TRUE ~ FALSE))
  
#OK, now subset to the surveys to use for the index and calculate CPUE
DSCPUE20 = twentyzeros %>%
  mutate(Year = year(Date)) %>%
  left_join(AvgDSlength) %>%
  filter(Station %in% index_stations$Station,
         Taxa == "Hypomesus transpacificus", Length <60,
         UseforIndex == TRUE) %>%
  group_by(Station, Survey, Year, Tow_volume) %>%
  summarize(Count = sum(Count)) %>%
  mutate(CPUE = Count/Tow_volume*10000)
 
#index is the geometric mean of CPUE 
DSCPUEmean = DSCPUE20 %>%
  mutate(logcpue = log(CPUE +1, 10)) %>%
  group_by(Year, Survey) %>%
  summarize(GeoMean = 10^(mean(logcpue))-1)

 
DSIndex20 = group_by(DSCPUEmean, Year) %>%
  summarize(Index = sum(GeoMean))

#Ugh, I give up, i'll just copy them from the memo

DSIndex20 = read_csv("data/20mm/20mm_DSM_index.csv")


##########################################################################
## Longfin Smelt
# 


#OK, now subset to the surveys to use for the index and calculate CPUE
LFCPUE20 = twentyzeros %>%
  mutate(Year = year(Date)) %>%
  filter(Station %in% index_stations$Station,
         Taxa == "Spirinchus thaleichthys", 
         Survey %in% c(1:4)) %>%
  group_by(Station, Survey, Year, TowNum, Tow_volume) %>%
  summarize(Count = sum(Count, na.rm =T))%>%
  mutate(CPUE = Count/Tow_volume*10000) 

LFCPUE20x = LFCPUE20 %>%
  group_by(Station, Survey, Year) %>%
  summarize(Count = mean(Count, na.rm =T), CPUE = mean(CPUE, na.rm =T))%>%
  mutate(logcpue = log(CPUE +1, 10))

#index is the some of the geometric mean of CPUE 
LFCPUEmean = LFCPUE20x  %>%
  group_by(Year, Survey) %>%
  summarize(meanCPUE = mean(logcpue, na.rm =T)) %>%
  mutate(GeoMean = 10^meanCPUE-1)


LSIndex20 = group_by(LFCPUEmean, Year) %>%
  summarize(Index = sum(GeoMean))

##########################################################################
save(DSIndex20, LSIndex20, file = "data/20mm.RData")
