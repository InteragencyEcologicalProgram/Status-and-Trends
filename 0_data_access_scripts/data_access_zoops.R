#script to download zooplankton data and get it ready to use.

source("setup.R")

library(readxl)
library(tidyverse)
library(smonitr)

zoops = download_file("https://filelib.wildlife.ca.gov/Public/IEP_Zooplankton/1972-2021CBMatrix.xlsx")
zoopscb = read_excel(zoops, guess_max = 10000, sheet = "CB CPUE Matrix 1972-2021")
#data is stored on the CDFW FTP site

zoopscb = rename(zoopscb, Station = StationNZ)

#pump data
zoopps = download_file("https://filelib.wildlife.ca.gov/Public/IEP_Zooplankton/1972-2021PumpMatrix.xlsx")
zoopsp = read_excel(zoopps, guess_max = 10000, sheet = "Pump CPUE Matrix 1972-2021")
zoopps = rename(zoopsp, Station = StationNZ)

save(zoopps, zoopscb, file = file.path(data_root,"Zoops.RData"))


#Mysid data
mysids = download_file("https://filelib.wildlife.ca.gov/Public/IEP_Zooplankton/1972-2021MacroMatrix.xlsx")
Mysids = read_excel(mysids, guess_max = 10000, sheet = "Macro CPUE Matrix 1972-2021")
Mysids = rename(Mysids, Station = StationNZ)

Mysidsb = read_excel(mysids, guess_max = 10000, sheet = "Macro lengths 1972-2021")
Mysidsb = rename(Mysidsb, Station = StationNZ) %>%
  mutate(Taxname = case_when(SpeciesName %in% c("Gammarus daiberi", "Unidentified Gammarus", "Crangonyx sp", "Grandidierella japonica", "Amithoe sp", "Hyalella sp")~ "Gammarus",
                             SpeciesName == "Unidentified Amphipod" ~ "Amphipoda",
                             SpeciesName %in% c("Unidentified Corophium", "Monocorophium sp") ~ "Corophiidae",
                             SpeciesName %in% c("Deltamysis holmquistae", "Acanthomysis hwanhaiensis", 
                                                "Unidentified Mysid", "Alienacanthomysis macropsis") ~ "Neomysis mercedis",
                                                                                            TRUE ~ SpeciesName))

conversions = read_excel("data/ZoopSynth_biomass_CEBupdated.xlsx", sheet = "Macro-zooplankton")
#mysid BPUE
                                                          
#conversions = filter(conversions, Preservative != "Ethanol")  

#assume dry weight is 10% wet weight
Mysidsbc = left_join(Mysidsb, conversions) %>%
  mutate(BPUE = (a*Size^b)*AdjustedFreq, weightper = a*Size^b) %>%
  #filter(Preservative != "Ethanol") %>%
  mutate(BPUEdry = case_when(Weight_type == "Wet" ~ BPUE* .1,
         TRUE ~ BPUE),
         Weightperdry = case_when(Weight_type == "Wet" ~ weightper*.1,
                                  TRUE ~ weightper))

test = select(Mysidsbc, Size, weightper, Weight_type, Weightperdry, Taxname, Preservative) %>%
filter(!is.na(weightper)) %>%
  distinct() 
#I"m all kinds of confused 


ggplot(test, aes(x = log(Size), y = log(Weightperdry), color = Preservative, linetype = Weight_type)) + geom_point()  + geom_line() +
  facet_wrap(~Taxname)

save(zoopps, zoopscb, Mysids, MysidBPUE, file = file.path(data_root,"Zoops.RData"))

test2 = filter(Mysidsbc, is.na(weightper))


test = select(Mysidsbc, Size, weightper, Weightperdry, Taxname) %>%
  filter(!is.na(weightper)) %>%
  distinct() 
