
library(readxl)

# These strings may need to be changed for each download
url <- "ftp://ftp.wildlife.ca.gov/BayStudy/CatchMatrices/BayStudy_1980-2018_FishMatrices.zip"
mwt_file <- "BayStudy_MWT_1980-2018_FishMatrix.xlsx"
ot_file <- "BayStudy_OT_1980-2018_FishMatrix.xlsx"

# Create some tempfiles to temporarily hold data
temp <- tempfile()
temp2 <- tempfile()

# Download data from the FTP site and store in temp file
download.file(url, temp)

# Unzip data and store in temp2 file
unzip(zipfile = temp, exdir = temp2)

# Extract fish matrix data from temp2 file
  # Midwater trawl
  mwt_data <- read_excel(file.path(temp2, mwt_file), sheet = "MWT_Catch_Matrix")
  
  # Otter trawl
  ot_data <- read_excel(file.path(temp2, ot_file), sheet = "OT_Catch_Matrix")
  
# Remove temp files
unlink(c(temp, temp2))
rm(temp, temp2)
