# Purpose: Testing smonitr v2_dev functions
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(lubridate)
library(readxl)
library(smonitr) #v2_dev is installed on my machine


# Download Bay Study Fish Data --------------------------------------------

ftp_url <- "ftp://ftp.wildlife.ca.gov"
ftp_dir_path <- "BayStudy/CatchMatrices"
zip_name <- "FishMatrices"
file_names <- c("MWT", "OT")
sheet_names <- c("MWT_Catch_Matrix", "OT_Catch_Matrix")

zip_f <- get_ftp_data(ftp_url, ftp_dir_path, zip_name, download_file)
zip_extract_f <- extract_files(zip_f[[1]], file_names, exdir = tempdir())

bs_fish_data <-
  map2(zip_extract_f, sheet_names, .f = read_excel) %>%
  set_names(c("mwt", "ot"))

# Clean Longfin Smelt Data and Calculate CPUE ----------------------------------------

# Clean and modify data
lonsme_clean <- bs_fish_data$mwt %>%
  select(Year:ChanShoal, TowVolume, LONSME = "LONSMEAge1+") %>%
  mutate(
    Month = month(Date, label = TRUE, abbr = TRUE),
    YearIndex = if_else(
      Month %in% c("Jan", "Feb", "Mar"),
      Year - 1,
      Year
    )
  ) %>%
  # Used Survey = 1, 2, and 12 instead of month to include complete dataset
  filter(Survey %in% c(1, 2, 12)) %>%
  # Don't include years 1979 and 2018
  filter(!YearIndex %in% c(1979, 2018))

# Calculate CPUE values & average them for each year
lonsme_cpue <- lonsme_clean %>%
  mutate(CPUE = (LONSME/TowVolume) * 10000) %>%
  group_by(YearIndex) %>%
  summarize(AvgCPUE = mean(CPUE))

# Create Plots ---------------------------------------------------------

lonsme_cpue %>% standard_column_plot(aes(x = YearIndex, y = AvgCPUE), 2017, "winter", "all")

lonsme_cpue %>% standard_column_plot(aes(x = YearIndex, y = AvgCPUE), 2017, "winter", "recent")


