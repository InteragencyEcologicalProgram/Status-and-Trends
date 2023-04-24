# Purpose: 
  # Download fish catch matrices for the Bay Study from the FTP site
  # Export data to SharePoint site as an .RData file to be used in other scripts

# Author: Dave Bosworth

# Bay Study website: https://wildlife.ca.gov/Conservation/Delta/Bay-Study
# Bay Study FTP site: https://filelib.wildlife.ca.gov/Public/BayStudy/

source("setup.R")

# Load readxl package
library(readxl)

# This is the URL for the zip file to download
# **** The end date in the zip file name will probably change in the future ****
baystudy_url <- "https://filelib.wildlife.ca.gov/Public/BayStudy/CatchMatrices/Bay Study Fish Matrices 1980-2020.zip"

# Create some tempfiles to temporarily hold data
temp1 <- tempfile()
temp2 <- tempfile()

# Download data from the FTP site and store in temp1 file
download.file(url = baystudy_url, destfile = temp1)

# Unzip data and store in temp2 file
unzip(zipfile = temp1, exdir = temp2)

# These are the two xlsx files to be extracted from the zip file
# **** The end dates in both file names will probably change in the future ****
mwt_file <- "BayStudy_MWT_1980-2020_Fish Matrix.xlsx"
ot_file <- "BayStudy_OT_1980-2020_FishMatrix.xlsx"

# Extract fish matrix data from temp2 file
# **** I assume that the sheet names won't change in the future ****
  # Midwater trawl
  midwater_trawl_data <- read_excel(file.path(temp2, mwt_file), sheet = "MWT_Catch_Matrix")

  # Otter trawl
  otter_trawl_data <- read_excel(file.path(temp2, ot_file), sheet = "OT_Catch_Matrix")
  
# Remove temp files
unlink(c(temp1, temp2))
rm(temp1, temp2)

# Create a list containing metadata for the data files
metadata <- list(
  original_file = baystudy_url,
  date_downloaded = Sys.Date()
)  

# Export Bay Study fish data to SharePoint site as an .RData file
save(midwater_trawl_data, otter_trawl_data, metadata, file = paste0(data_root, "/BayStudyFish.RData"))

