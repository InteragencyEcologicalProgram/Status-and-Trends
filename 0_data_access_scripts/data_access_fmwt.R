#script to download fall-midwater-trawl data and get it ready to use.

source("setup.R")

library(readxl)
library(tidyverse)
library(smonitr)


#CDFW started just putting their indexes in a spreadsheet Much easier
FMWT = download_file("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/FMWTindices.csv")
FMWT1 = read_csv(FMWT, guess_max = 10000)

write.csv(FMWT1, file.path(data_root,"fmwt.csv"), row.names = F)
