#script to download zooplankton data and get it ready to use.

source("setup.R")

library(readxl)
library(tidyverse)
library(smonitr)

zoops = download_file("https://filelib.wildlife.ca.gov/Public/IEP_Zooplankton/1972-2024_CBMatrix_Public.xlsx")
zoopscb = read_excel(zoops, guess_max = 100000, sheet = "1972-2024_CB_Matrix")

#data is stored on the CDFW FTP site

zoopscb = rename(zoopscb, Station = StationNZ)

#pump data

zoopps = download_file("https://filelib.wildlife.ca.gov/Public/IEP_Zooplankton/1972-2024_PumpMatrix_Public.xlsx")
excel_sheets(zoopps)
zoopsp = read_excel(zoopps, guess_max = 100000, sheet = "1972-2024_Pump_CPUE_Matrix")

zoopps = rename(zoopsp, Station = StationNZ)

save(zoopps, zoopscb, file = file.path(data_root,"Zoops.RData"))


#Mysid data

mysids = download_file("https://filelib.wildlife.ca.gov/Public/IEP_Zooplankton/1972-2024_MysidMatrix_Public.xlsx")
excel_sheets(mysids)
Mysids = read_excel(mysids, guess_max = 100000, sheet = "1972-2024_Mysid_Matrix")
Mysids = rename(Mysids, Station = StationNZ)


#Length data from edi, i guess
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.522.14


Mysidsb = read_csv("data/1972-2024_Mysid_Length_Data_EDI.csv", guess_max = 10000)
Mysidsb = rename(Mysidsb, Station = StationNZ) %>%
  mutate(Taxname = case_when(SpeciesName %in% c("Gammarus daiberi", "Unidentified Gammarus", "Crangonyx sp", "Grandidierella japonica", "Amithoe sp", "Hyalella sp")~ "Gammarus",
                             SpeciesName == "Unidentified Amphipod" ~ "Amphipoda",
                             SpeciesName %in% c("Unidentified Corophium", "Monocorophium sp") ~ "Corophiidae",
                             SpeciesName %in% c("Deltamysis holmquistae", "Acanthomysis hwanhaiensis", 
                                                "Unidentified Mysid", "Alienacanthomysis macropsis") ~ "Neomysis mercedis",
                                                                                            TRUE ~ SpeciesName),
         SampleDate = mdy(SampleDate))

conversions = read_excel("data/ZoopSynth_biomass_CEBupdated.xlsx", sheet = "Macro-zooplankton")
#mysid BPUE
                                                          
conversions = filter(conversions, Preservative == "Formalin") %>%
  filter(!(Taxname == "Hyperacanthomysis longirostris" & Weight_type == "Wet"))

#assume dry weight is 10% wet weight
#assume carbon weight is 0.4 times dry weight
#then covert to micro-grams to match the mesozoo[s]
#AND DIVIDE BY VOLUME
volumes = select(Mysids, SampleDate, Station, Year, Volume)

Mysidsbc = left_join(Mysidsb, conversions) %>%
  left_join(volumes) %>%
  mutate(BPUE = (a2*(Length^b))*AdjustedFreq/Volume, weightper = a2*(Length^b)) %>%
  #filter(Preservative == "Formalin", Weight_type == "Dry") %>%
  mutate(BPUEdryC = case_when(Weight_type == "Wet" ~ BPUE* .1*.4,
                              Weight_type == "Dry"~ BPUE*.4,
         TRUE ~ BPUE),
         #multiply by 1000 to convert 
         bpue = BPUEdryC*1000)



testa=  filter(mmass, Year == 2010) %>%
  select(Station, Year, Survey, Date, Hyperacanthomysis.longirostris) %>%
  mutate(SampleDate = mdy(Date))

testb = filter(Mysidsbc, year(SampleDate) == 2010, Taxname == "Hyperacanthomysis longirostris") %>%
  group_by(SampleDate, Station) %>%
  summarize(BPUEtot = sum(bpue), BPUEdryC = sum(BPUEdryC), BPUEx = sum(BPUE))

testc = left_join(testa, testb) %>%
  mutate(bpue2 = Hyperacanthomysis.longirostris*1000)

save(zoopps, zoopscb, Mysidsbc, file = file.path(data_root,"Zoops.RData"))
