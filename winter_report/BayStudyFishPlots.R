# Winter 2017 Status and Trends Report
# Bay Study
# Create plots for longfin smelt and sturgeon 

library(tidyverse)
library(readxl)
library(lubridate)

# Define defaults
  # Start year for plots- all years
  startYearAll <- 1966
  # Start year for plots- recent trends
  startYearRec <- 2003
  # End year for x-axis limit
  endYear <- 2020

# Longfin smelt
  # Import Dataset
  lonsme.orig <- 
    read_excel(path = "data/Bay Study_MWT_1980-2018_FishMatrix.xlsx") 
  
  # Clean and modify longfin.orig df
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
    ungroup()

# White sturgeon
  # Import Jason's calculated YCI dataset
  whistu.orig <- 
    read_csv("data/yci_bs.csv") 
  
  # Clean and modify whistu.orig df
  whistu.clean <- whistu.orig %>% 
    filter(Year != 2018) %>% 
    select(Year, WYCI) %>% 
    rename(
      Year.Index = Year,
      y.val = WYCI
    )
  
# Create a tibble of long-term averages of either CPUE or WYCI
lt.avg <- tibble(
  FishSpecies = c("lonsme", "whistu"),
  Average = c(mean(lonsme.cpue$y.val), mean(whistu.clean$y.val))
)

# Create placeholder data to add to datasets so that the x-axes of the plots start at 1966
yearsBefore <- tibble(
  Year.Index = seq(1966, 1979, 1),
  y.val = 0
)

# Create placeholder for missing longfin smelt data in 1994
yearsMiss.lonsme <- tibble(
  Year.Index = 1994,
  y.val = 0
)

# Combine df's for each species into a nested dataframe for more efficient plotting
plot.dat <- 
  list(
    lonsme = lonsme.cpue,
    whistu = whistu.clean
  ) %>% 
  # Add placeholder data
  map(~bind_rows(yearsBefore, .x)) %>% 
  map_at("LongfinSmelt", ~bind_rows(.x, yearsMiss.lonsme)) %>% 
  # Consolidate data into on df with id as FishSpecies
  bind_rows(.id = "FishSpecies") %>% 
  # Make Year.Index variable a factor for plot order
  # mutate(Year.Index = factor(Year.Index, levels = yearFactor)) %>% 
  group_nest(FishSpecies) %>% 
  # Add long-term averages to df
  left_join(lt.avg) %>% 
  # Rename data variable
  rename(data.all = data) %>% 
  # Create a new data variable for just recent trends data
  mutate(data.recent = map(data.all, ~filter(.x, Year.Index >= startYearRec))) %>% 
  select(FishSpecies, Average, data.all, data.recent)
  


# Create functions for the plots
  # Base Plot
  f_PlotBase <- function(df) {
    
    # Create base plot p
    p <- 
      ggplot(
        data = df,
        aes(
          x = Year.Index,
          y = y.val
        )
      ) +
      geom_col() +
      theme_smr()
    
    return(p)
  }

  #Different colors based on fish species  
  f_columns <- function(p, FSpec) {
    
    p <- p +
      geom_col(aes(fill = FSpec)) +
      scale_fill_manual(values = c("lonsme" = "#664F2B", "whistu" = "#748D83")) +
      guides(fill = F)
    
    return(p)
  }

  # Add a dashed line for long term average
  f_PlotLtAvg <- function(p, Avg) {
    
    p <- p +
      geom_hline(
        yintercept = Avg, 
        color = "red",
        linetype = "dashed", 
        size = 0.9
      )
    
    return(p)
  }
  
  # Set axis breaks and limits
  f_PlotModAxis <- function(p, FSpec, PlotType) {
    
    # define x-axis breaks
    yearBreaksAll <- seq(1970, endYear, 10)
    yearBreaksRec <- seq(2005, endYear, 5)
    
    # define y-axis limits
    all.lonsme <- 40
    all.whistu <- 720
    rec.lonsme <- 8
    rec.whistu <- 300
    
    # Plots for all years
    if (PlotType == "All") {
      # Set x-axis breaks
      p <- p + scale_x_continuous(breaks = yearBreaksAll)
      
      # Set axis limits- lonsme
      if (FSpec == "lonsme") {
        p <- p +
          coord_cartesian(
            xlim = c(startYearAll, endYear),
            ylim = c(0, all.lonsme)
          )
        
        # Set axis limits- whistu
      } else if (FSpec == "whistu") {
        p <- p +
          coord_cartesian(
            xlim = c(startYearAll, endYear),
            ylim = c(0, all.whistu)
          )
      }
      
      # Plots for Recent years
    } else if (PlotType == "Recent") {
      # Set x-axis breaks
      p <- p + scale_x_continuous(breaks = yearBreaksRec)
      
      # Set axis limits- lonsme
      if (FSpec == "lonsme") {
        p <- p +
          coord_cartesian(
            xlim = c(startYearRec, endYear),
            ylim = c(0, rec.lonsme)
          )
        
        # Set axis limits- whistu
      } else if (FSpec == "whistu") {
        p <- p +
          coord_cartesian(
            xlim = c(startYearRec, endYear),
            ylim = c(0, rec.whistu)
          )
      }
    }
    
    return(p)
  }
  
  # Add axis labels
  f_PlotAxisLab <- function(p, FSpec) {
    
    # labels for lonsme plots
    if (FSpec == "lonsme") {
      p <- p + 
        labs(
          x = "Year (December - February)",
          y = expression(paste("Average CPUE (fish/10,000m"^{3}, ")"))
        )
      
    } else if (FSpec == "whistu") {
      p <- p +
        labs(
          x = "Year",
          y = "Year Class Index"
        )
    }
    
    return(p)
  }
  
  # Add descriptive text to the plots for all years
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
    
    # Add text to whistu plots 
    } else if (FSpec == "whistu") {
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
    
    return(p)
  }
  
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

# Run final plotting function on nested df
fish.plots <- plot.dat %>% 
  mutate(
    plot_allYears = pmap( 
      list(data.all, FishSpecies, Average),
      f_PlotFinal, 
      PlotType = "All"
    ),
    plot_recent = pmap(
      list(data.recent, FishSpecies, Average),
      f_PlotFinal, 
      PlotType = "Recent"
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

