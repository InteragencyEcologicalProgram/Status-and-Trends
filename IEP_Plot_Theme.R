# Seasonal Monitoring Report
# Purpose: A group of common functions to apply consistency across plots
# Author: Dave Bosworth


# Function to customize plot formatting
#' Custom formatting for plots in the IEP Seasonal Monitoring Reports
#' This function builds off of the theme_bw() ggplot theme and removes plot gridlines and modifies the text size of various plot elements.
#' @export
theme_iep <- function() {
  theme_bw() +
  theme(
    # Define text size of tick labels
    axis.text = element_text(size = 9),
    # Define text size and face of axes labels; modify margins of y-axis label
    axis.title.x = element_text(
      size = 10, 
      face = "plain"
    ),
    axis.title.y = element_text(
      size = 10,
      face = "plain",
      margin = margin(0, 10, 0, 0) #top, right, bottom, left
    ), 
    # Remove panel grid lines
    panel.grid = element_blank(),
    # Adjust the margin dimensions so nothing is cut off
    plot.margin = unit(
      c(0.25, 0.6, 0.1, 0.4), #top, right, bottom, left
      units = "cm"
    ),
    # Define text size and justification of plot title
    plot.title = element_text(
      size = 20, 
      vjust = 1, 
      hjust = 0.5
    ),
    # Define text size and face of legend item labels
    legend.text = element_text(
      size = 9, 
      face = "plain"
    ),
    # Define text size of legend title
    legend.title=element_text(size = 10)
  )
}



