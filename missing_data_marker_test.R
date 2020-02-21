# Seasonal Monitoring Report
# Purpose: Test out various markers to represent years with missing data
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(scales)
library(smonitr)

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


# 3. Create Plots ---------------------------------------------------------

# Calculate long-term average of CPUE
lt_avg_cpue <- mean(noranc_cpue$ave_cpue)

# Create a dataframe of all possible years
all_yrs <- tibble(
  years = seq(min(noranc_cpue$Year), report_year, 1),
  result = 0
)

# Find missing years in noranc_cpue df
missing_yrs <- anti_join(all_yrs, noranc_cpue, by = c("years" = "Year"))

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
  theme_smr() +
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
  std_x_axis_all_years(report_year)

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
  theme_smr() +
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
  std_x_axis_rec_years(report_year)

# Try out different markers
# Filled markers
noranc_plot_all_test <- 
  noranc_plot_all +
  geom_point(
    data = missing_yrs,
    aes(
      x = years,
      y = result
    ),
    inherit.aes = FALSE,
    na.rm = TRUE,
    shape = 24,
    size = 1,
    fill = "dodgerblue",
    color = "gray10"
  )

noranc_plot_rec_test <- 
  noranc_plot_rec +
  geom_point(
    data = missing_yrs,
    aes(
      x = years,
      y = result
    ),
    inherit.aes = FALSE,
    na.rm = TRUE,
    shape = 24,
    size = 2,
    fill = "dodgerblue",
    color = "gray10"
  )

# Unfilled markers
noranc_plot_all_test <- 
  noranc_plot_all +
  geom_point(
    data = missing_yrs,
    aes(
      x = years,
      y = result
    ),
    inherit.aes = FALSE,
    na.rm = TRUE,
    shape = 8,
    size = 1,
    color = "dodgerblue"
  )

noranc_plot_rec_test <- 
  noranc_plot_rec +
  geom_point(
    data = missing_yrs,
    aes(
      x = years,
      y = result
    ),
    inherit.aes = FALSE,
    na.rm = TRUE,
    shape = 8,
    size = 2,
    color = "dodgerblue"
  )


# 4. Print plots ----------------------------------------------------------

# Plot for all years
ggsave(
  plot = noranc_plot_all_test,
  filename = "noranc_all_years.png",
  dpi = 300,
  units = "cm",
  width = 9.3,
  height = 6.8
)

# Plot for recent years
ggsave(
  plot = noranc_plot_rec_test,
  filename = "noranc_rec_years.png",
  dpi = 300,
  units = "cm",
  width = 9.3,
  height = 6.8
)
