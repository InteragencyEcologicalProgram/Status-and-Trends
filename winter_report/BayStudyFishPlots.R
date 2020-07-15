# Winter 2017 Status and Trends Report
# Bay Study
# Create plots for longfin smelt and sturgeon 

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(smonitr)

# Define report year
report_year <- 2017

# 1. Import Data ----------------------------------------------------------

# Longfin smelt
lonsme.orig <- read_excel(path = "data/Bay Study_MWT_1980-2018_FishMatrix.xlsx") 

# White sturgeon - Jason's calculated YCI dataset
whistu.orig <- read_csv("data/yci_bs.csv") 


# 2. Clean Data and Calculate CPUE ----------------------------------------

# Longfin smelt
  # Clean and modify data
  lonsme.clean <- lonsme.orig %>% 
    select(Year:ChanShoal, TowVolume, LONSMEAge1) %>% 
    mutate(
      Month = month(Date, label = TRUE, abbr = TRUE),
      Year.Index = if_else(
        Month %in% c("Jan", "Feb", "Mar"),
        Year - 1,
        Year 
      )
    ) %>% 
    # Used Survey = 1, 2, and 12 instead of month to include complete dataset
    filter(Survey %in% c(1, 2, 12)) %>% 
    # Don't include years 1979 and 2018
    filter(!Year.Index %in% c(1979, 2018))
  
  # Calculate CPUE values & average them for each year
  lonsme.cpue <- lonsme.clean %>% 
    mutate(CPUE = (LONSMEAge1/TowVolume) * 10000) %>% 
    group_by(Year.Index) %>% 
    summarize(y.val = mean(CPUE)) %>% 
    ungroup() %>% 
    mutate(FishSpecies = "lonsme")

# White sturgeon
  # Clean and modify data and average YCI for each year
  whistu.clean <- whistu.orig %>% 
    filter(Year != 2018) %>% 
    select(Year, WYCI) %>% 
    rename(
      Year.Index = Year,
      y.val = WYCI
    ) %>% 
    mutate(FishSpecies = "whistu")

  
# 3. Set up options for plots ---------------------------------------------

# Create a tibble of long-term averages of either CPUE or WYCI
lt.avg <- tibble(
  FishSpecies = c("lonsme", "whistu"),
  Average = c(mean(lonsme.cpue$y.val), mean(whistu.clean$y.val))
)
  
# Create dataframes for text comments on plots
lonsmeText.h <- tibble(
  Year.Index = as.factor(1966),
  yValue = 1,
  label = "Data were not collected\nuntil 1980"
)

lonsmeText.v <- tibble(
  Year.Index = as.factor(1983),
  yValue = 27,
  label = "Ave CPUE was 88 in 1982"
)

whistuText <- tibble(
  Year.Index = as.factor(1966),
  yValue = 20,
  label = "Data were not collected\nuntil 1980"
)


# 4. Create Plots ---------------------------------------------------------  

# Create function for the plots
plot_fish_data <- function(df, f_spec = c("lonsme", "whistu"), lt_avg, plot_type = c("all", "recent")) {
  
  f_spec <- match.arg(f_spec, c("lonsme", "whistu"))
  plot_type <- match.arg(plot_type, c("all", "recent"))
  
  # Create base plot
  p <- 
    ggplot(
      data = df,
      aes(
        x = Year.Index,
        y = y.val
      )
    ) +
    # apply custom theme
    theme_smr() +
    # add markers for missing data
    missing_data_symb(df, Year.Index, report_year, 0.9)
  
  # Add different options based on f_spec argument
  if (f_spec == "lonsme") {
    p <- p +
      #custom color for barplot
      geom_col(fill = "#664F2B") +
      # add labels for axes
      std_x_axis_label("winter") +
      ylab(expression(paste("Average CPUE (fish/10,000m"^{3}, ")")))
  } else {
    p <- p +
      #custom color for barplot
      geom_col(fill = "#748D83") +
      # add labels for axes
      xlab("Year") +
      ylab("Year Class Index")
  }
  
  # Standardize the x-axis limits and breaks for plots based on plot_type argument
  if (plot_type == "all") {
    p <- p + std_x_axis_all_years(report_year, "discrete")
  } else {
    p <- p + std_x_axis_rec_years(report_year, "discrete")
  }
  
  # Change the y-axis limits for the lonsme plots
  if (f_spec == "lonsme") {
    if (plot_type == "all") {
      p <- p + coord_cartesian(ylim = c(0, 40))
    } else {
      p <- p + coord_cartesian(ylim = c(0, 8))
    }
  }
  
  # Add descriptive text to the plots for all years
<<<<<<< HEAD
  f_PlotAddText <- function(p, FSpec) {
    
    # Define text size
    textSize <- 1.5
    
    # Create df's for text comments on plots
    lonsmeText.h <- tibble(
      Year.Index = 1964,
      yValue = 1,
      label = "Data were not collected\nuntil 1980"
    )
    
    lonsmeText.v <- tibble(
      Year.Index = c(1983),
      yValue = c(27),
      label = c(
        "Ave CPUE was 88 in 1982"
      )
    )
    
    whistuText <- tibble(
      Year.Index = 1964,
      yValue = 20,
      label = "Data were not collected\nuntil 1980"
    )
    
    # Add text to lonsme plots
    if (FSpec == "lonsme") {
=======
  textSize <- 1.5
  
  if (plot_type == "all") {
    if (f_spec == "lonsme") {
>>>>>>> bf83e48d8f971f0d729f4792b55b53143c5a061e
      p <- p +
        geom_text(
          data = lonsmeText.h,
          aes(
            x = Year.Index,
            y = yValue,
            label = label
          ),
          inherit.aes = FALSE,
          hjust = "left",
          size = textSize
        ) +
        geom_text(
          data = lonsmeText.v,
          aes(
            x = Year.Index,
            y = yValue,
            label = label
          ),
          inherit.aes = FALSE,
          hjust = "left",
          vjust = "center",
          size = textSize,
          angle = 90
        )
    } else {
      p <- p +
        geom_text(
          data = whistuText,
          aes(
            x = Year.Index,
            y = yValue,
            label = label
          ),
          inherit.aes = FALSE,
          hjust = "left",
          size = textSize
        )
    }
  }
  
<<<<<<< HEAD
  # Final plotting function
  f_PlotFinal <- function(df, FSpec, Avg, PlotType) {
    
    pFinal <- f_PlotBase(df) %>% 
      f_columns(FSpec) %>%
      f_PlotLtAvg(Avg) %>% 
      f_PlotModAxis(FSpec, PlotType) %>% 
      f_PlotAxisLab(FSpec) %>% 
      f_PlotAddText(FSpec) %>%
      missing_data_symb(Year.Index, 2018, 1)
    
    return(pFinal)
  }
=======
  # add horizontal line for long-term average CPUE - at the end to put line on top of bars
  p <- p + lt_avg_line(lt_avg)
  
  return(p)
}
>>>>>>> bf83e48d8f971f0d729f4792b55b53143c5a061e

# Combine df's for each species into a nested dataframe for more efficient plotting
fish.plots <- 
  bind_rows(lonsme.cpue, whistu.clean) %>% 
  # Make Year.Index variable a factor for plot order
  mutate(Year.Index = factor(Year.Index)) %>% 
  group_nest(FishSpecies) %>% 
  # Add long-term averages to df
  left_join(lt.avg) %>% 
  select(FishSpecies, Average, data) %>% 
  # Create plots
  mutate(
<<<<<<< HEAD
    plot_allYears = pmap( 
      list(data.all, FishSpecies, Average),
      f_PlotFinal, 
      PlotType = "All"
=======
    plot_allYears = pmap(
      list(data, FishSpecies, Average),
      plot_fish_data, 
      plot_type = "all"
>>>>>>> bf83e48d8f971f0d729f4792b55b53143c5a061e
    ),
    plot_recent = pmap(
      list(data, FishSpecies, Average),
      plot_fish_data, 
      plot_type = "recent"
    )
  )


# Restructure fish.plots df for printing off plots
fish.plots.print <- fish.plots %>% 
  select(FishSpecies, plot_allYears, plot_recent) %>% 
  pivot_longer(
    cols = c(plot_allYears, plot_recent),
    names_to = "PlotType",
    values_to = "Plots"
  ) %>% 
  unite("FileName", FishSpecies, PlotType)

# Export plots
walk2(
  fish.plots.print$Plots,
  fish.plots.print$FileName,
  ~ggsave(
    plot = .x,
    filename = paste0(.y, ".png"),
    path = "docs/figures/",
    dpi = 300,
    units = "cm",
    width = 9.3,
    height = 6.8
  )
)

