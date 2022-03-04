#script to download zooplankton data and get it ready to use.

library(readxl)
library(tidyverse)
library(smonitr)

zoops = download_file("https://filelib.wildlife.ca.gov/Public/IEP_Zooplankton/1972-2020CBMatrix.xlsx")
zoopscb = read_excel(zoops, guess_max = 10000, sheet = "CB CPUE Matrix 1972-2020")
#data is stored on the CDFW FTP site

zoopscb = rename(zoopscb, Station = StationNZ)

#pump data
zoopps = download_file("https://filelib.wildlife.ca.gov/Public/IEP_Zooplankton/1972-2020PumpMatrix.xlsx")
zoopsp = read_excel(zoopps, guess_max = 10000, sheet = "Pump CPUE Matrix 1972-2020")
zoopps = rename(zoopsp, Station = StationNZ)

save(zoopps, zoopscb, file = file.path(data_root,"Zoops.RData"))


#Mysid data
mysids = download_file("https://filelib.wildlife.ca.gov/Public/IEP_Zooplankton/1972-2020MysidMatrix.xlsx")
Mysids = read_excel(mysids, guess_max = 10000, sheet = "Mysid CPUE Matrix 1972-2020")
Mysids = rename(Mysids, Station = Mysids)

#mysid BPUE
MysidBPUE <- read_excel("data/1972-2020MysidBPUEMatrix.xlsx", 
                                                     sheet = "Mysid_BPUE_matrix_1972-2020", 
                                                     col_types = c("numeric", "numeric", "numeric", 
                                                                   "numeric", "date", "text", "date", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric", 
                                                                   "numeric", "numeric", "numeric"))

save(zoopps, zoopscb, Mysids, MysidBPUE, file = file.path(data_root,"Zoops.RData"))

