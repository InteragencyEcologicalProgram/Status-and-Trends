
source("setup.R")

library(glue)
library(purrr)
library(readr)
library(stringr)


# Download the YBFMP Files
ybfmp = get_edi_data(233, c("Integrated Water Quality and Fish Catch",
  "Fish Taxonomy", "Sampling Effort", 
  "Stations"), guess_max = 100000)

# Path to sharepoint site
#sharepoint_path = normalizePath(
#  file.path(
#    Sys.getenv("USERPROFILE"),
#    "California Department of Water Resources/DWR - Seasonal monitoring report - Documents/Data"
#  )
#)  

# save data  
#save(ybfmp, file = file.path(sharepoint_path, "ybfmp.RData"))

save(ybfmp, file = file.path(data_root, "ybfmp.RData"))
