# Summer 2018 Seasonal Monitoring Report
# Purpose: Create plots of Northern Anchovy CPUE from the Bay Study dataset
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

# Define report year
report_year <- 2018

# 1. Import Data ----------------------------------------------------------
# Dataset is on SharePoint site for the Seasonal Monitoring Report

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - Seasonal monitoring report - Documents/Data"
  )
)  

# Import fish data
load(file = paste0(sharepoint_path, "/BayStudyFish.RData"))


# 2. Clean Data and Calculate CPUE ----------------------------------------

# Clean and modify midwater_trawl_data df
noranc <- midwater_trawl_data %>% 
  select(Year:ChanShoal, TowVolume, NORANC) %>% 
  # Used Survey = 6, 7, 8 instead of month to include complete dataset
  filter(Survey >= 6 & Survey <= 8) %>% 
  # remove data from July 2016 due to invalid tow data
  filter(!(Year == 2016 & Survey == 7))

# Calculate CPUE values & average them for each year
noranc_cpue <- noranc %>% 
  mutate(cpue = (NORANC/TowVolume) * 10000) %>% 
  group_by(Year) %>% 
  summarize(ave_cpue = mean(cpue)) %>% 
  ungroup()


# 3. Set up options for plots ---------------------------------------------

# Calculate long-term average of CPUE
lt_avg_cpue <- mean(noranc_cpue$ave_cpue)

# Define text size for text comments on plots
text_size_all <- 1.5
text_size_rec <- 2

# Create dataframes for text comments on plots
noranc_text_h <- tibble(
  Year = 1964,
  yValue = 40,
  label = "Data were not collected\nuntil 1980"
)

noranc_text_v <- tibble(
  Year = c(1994, 2018),
  yValue = 0,
  label = "No data"
)

# Import functions for plots
source("IEP_Plot_Theme.R")


# 3. Create Plots ---------------------------------------------------------

# Plot for all years
noranc_plot_all <- noranc_cpue %>% 
  ggplot(
    aes(
      x = Year,
      y = ave_cpue
    )
  ) +
  geom_col() +
  # apply custom theme
  theme_iep() +
  # customize axis labels
  labs(
    x = "Year (June - August)",
    y = expression(paste("Average CPUE (fish/10,000m"^{3}, ")"))
  ) +
  # define y-axis breaks and add thousanths comma
  scale_y_continuous(
    breaks = seq(0, 1250, by = 250),
    labels = label_comma()
  ) +
  # add horizontal line for long-term average CPUE
  lt_avg_line(lt_avg_cpue) +
  # standardize x-axis
  std_x_axis_all_years(report_year) +
  # add horizontal text to plot
  geom_text(
    data = noranc_text_h,
    aes(
      x = Year,
      y = yValue,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = "left",
    size = text_size_all
  ) +
  # add vertical text to plot
  geom_text(
    data = noranc_text_v,
    aes(
      x = Year,
      y = yValue,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = "left",
    vjust = "center",
    size = text_size_all,
    angle = 90
  )

# Plot for recent years (15 years from report date)
noranc_plot_rec <- noranc_cpue %>% 
  filter(Year >= report_year - 14) %>% 
  ggplot(
    aes(
      x = Year,
      y = ave_cpue
    )
  ) +
  geom_col() +
  # apply custom theme
  theme_iep() +
  # customize axis labels
  labs(
    x = "Year (June - August)",
    y = expression(paste("Average CPUE (fish/10,000m"^{3}, ")"))
  ) +
  # custom y-axis breaks
  scale_y_continuous(breaks = seq(0, 500, by = 100)) +
  # add horizontal line for long-term average CPUE
  lt_avg_line(lt_avg_cpue) +
  # standardize x-axis
  std_x_axis_rec_years(report_year) +
  # add vertical text to plot
  geom_text(
    data = noranc_text_v,
    aes(
      x = Year,
      y = yValue,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = "left",
    vjust = "center",
    size = text_size_rec,
    angle = 90
  ) 


# Print Plots
  # Plot for all years
  ggsave(
    plot = noranc_plot_all,
    filename = "noranc_all_years.png",
    dpi = 300,
    units = "cm",
    width = 9.3,
    height = 6.8
  )

  # Plot for recent years
  ggsave(
    plot = noranc_plot_rec,
    filename = "noranc_rec_years.png",
    dpi = 300,
    units = "cm",
    width = 9.3,
    height = 6.8
  )

