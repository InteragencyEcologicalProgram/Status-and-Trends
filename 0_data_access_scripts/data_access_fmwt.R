#script to download fall-midwater-trawl data and get it ready to use.

library(readxl)
library(tidyverse)
library(smonitr)


#CDFW started just putting their indexes in a spreadsheet Much easier
FMWT = get_ftp_data("ftp://ftp.dfg.ca.gov", "TownetFallMidwaterTrawl/FMWT%20Data", "FMWTindices.csv",
             guess_max = 100000L)
FMWT = FMWT$FMWTindices.csv

write.csv(FMWT, file.path(data_root,"fmwt.csv"), row.names = F)
