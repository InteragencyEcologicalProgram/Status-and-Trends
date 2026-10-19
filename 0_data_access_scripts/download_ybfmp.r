
source("setup.R")

library(glue)
library(purrr)
library(readr)
library(stringr)

#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.233.6

# Download the YBFMP Files
ybfmpcatch = read_csv("bigdata/integrated_wq_totalcatch.csv")
ybfmptax =read_csv("bigdata/taxonomy.csv")
ybfmpeffort = read_csv("bigdata/effort.csv")

ybfmpeffort_hist = read_csv("bigdata/monthly_trap_effort1.csv")




save(ybfmpcatch, ybfmptax, ybfmpeffort, ybfmpeffort_hist, file = file.path(data_root, "ybfmp.RData"))
