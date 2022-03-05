
source("setup.R")

library(glue)
library(purrr)
library(readr)
library(stringr)


# Download the YBFMP Files
ybfmp = get_edi_data(233, c("YBFMP_Fish_Catch_and_Water_Quality.csv",
  "YBFMP_Fish_Taxonomy.csv", "YBFMP_Trap_Effort.csv",
  "YBFMP_Site_locations_latitude_and_longitude.csv"), guess_max = 100000)

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
