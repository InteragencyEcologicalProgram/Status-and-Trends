#Let's figure out how to go from lengths to biomass for mysids

source("setup.R")

library(tidyverse)
library(lubridate)
library(readxl)

#Mysid biomass is calculated using lengths, 
mysids = read_xlsx("data/mysid_amphipod_lengths2019.xlsx")

conversion = read.csv("data/Mysid_bimass_conversions.csv")

conversion = filter(conversion, Preservative == "Formalin", Type == "Dry")

MysidMatrix = Mysids

#biomass is a * length^b

mysids2 = inner_join(mysids, conversion, by = "SpeciesCode") %>%
  mutate(biomass = a*(Size)^b)

mysids3 = group_by(mysids2, SampleDate, StationNZ, SpeciesCode, Species) %>%
  summarize(count = length(ID), totMass = sum(biomass), meanMass = mean(biomass))

Mysids_long2019 = pivot_longer(MysidMatrix, 
                           cols = A_aspera:Unidentified_mysid, 
                           names_to = "Species", values_to = "count") %>%
  filter(Year == 2019)

Mys2019 = left_join(Mysids_long2019, mysids3, by = c("SampleDate", "StationNZ", "Species")) %>%
  mutate(bpue = count.x*meanMass, cpue = count.x)

Mys2019$bpue[which(Mys2019$count.x== 0)] = 0

Mys2019b = group_by(Mys2019,SampleDate, StationNZ) %>%
  summarise(bpue = sum(bpue, na.rm = T), cpue = sum(cpue, na.rm = T))

#give it all the names of the other dataset

Mys2019b = mutate(Mys2019b, survey = NA, year = year(SampleDate), taxon = "mys") %>%
  rename(date = SampleDate, station = StationNZ) %>%
  filter(!station %in% c("NZEZ2", "NZEZ6","NZEZ6SJR", "NZEZ2SJR"))

#mysid net survey data through 2018
mysid<-read.csv(file.path(data_root,"zoop_mysid.csv")) 
mysid$SampleDate = mdy(mysid$SampleDate)

#mysid shrimp biomass for all samples collected 
mmass<-read.csv(file.path(data_root,"zoop_mysid_mass.csv"))

mmass$bpue = rowSums(mmass[,20:27])

mmass2 = rename(mmass, year = Year, date = Date, station = Station, survey = Survey) %>%
  mutate(taxon = "mys", cpue = NA, date = mdy(date)) %>%
  select("date", "station", "bpue", "survey", "year", "taxon")

mysids = rbind(Mys2019b, mmass2)
write.csv(data_root, "mysid_biomass.csv", row.names = F)
save(mysids, file = file.path(data_root, "mysidbiomass.RData"))
