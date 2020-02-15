#' @title Custom formatting for ggplots in the IEP Seasonal Monitoring Reports
#' @description This custom theme builds off of the \code{theme_bw()} ggplot theme
#'     and removes plot gridlines and modifies the text size of various ggplot elements.
#' @import ggplot2
#' @export
theme_smr <- function() {
  theme_bw() +
  theme(
    # Define text size of tick labels
    axis.text = element_text(size = 9),
    # Define text size and face of axes labels
    axis.title = element_text(
      size = 10,
      face = "plain"
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


#' @title Add a horizontal line to a ggplot
#' @description Adds a horizontal dashed red line to a ggplot. Its intended use
#'     is to represent the provided long-term average of a dataset.
#' @param lt_avg The long-term average of the data
#' @return A horizontal dashed red line to a ggplot
#' @import ggplot2
#' @export
lt_avg_line <- function(lt_avg) {
  geom_hline(
    yintercept = lt_avg,
    color = "red",
    linetype = "dashed",
    size = 0.9
  )
}


#' @title Round an integer to the nearest multiple
#' @description Used to round a year value to the nearest multiple of a defined value. This is
#'     an internal function used to define x-axis breaks for plots.
#' @param num Integer to round
#' @param mult Mulitple value to round to
#' @keywords internal
#' @return A value rounded to the nearest multiple of the \code{mult} parameter
round_to_mult <- function(num, mult) {
  num_round <- round(num/mult) * mult
  return(num_round)
}


#' @title  Standardize x-axis elements for ggplot of all years
#' @description Standardizes the x-axis limits and breaks for a ggplot of all years of data.
#'     The x variable needs to be type "integer" representing years.
#' @param rpt_yr The user-defined report year for the Seasonal Monitoring Report. Must be an integer.
#' @param start_yr The start year that defines the minimum x-axis limit for the ggplot. Must be an integer. Default is 1966.
#' @return Two ggplot layers that define the x-axis limits and breaks based upon user-defined arguments
#' @import ggplot2
#' @export
std_x_axis_all_years <- function(rpt_yr, start_yr = 1966) {
  # Define year breaks
  year_breaks <- seq(
    from = round_to_mult(start_yr, 10),
    to = round_to_mult(rpt_yr, 10),
    by = 10
  )

  # Create a list of layers to add to ggplot object
  list(
    scale_x_continuous(breaks = year_breaks),
    coord_cartesian(xlim = c(start_yr, rpt_yr))
  )
}


#' @title  Standardize x-axis elements for ggplot of recent years
#' @description Standardizes the x-axis limits and breaks for a ggplot of recent years of data.
#'     The x variable needs to be type "integer" representing years. This function
#'     works best if the dataset is filtered to the necessary date range beforehand.
#' @param rpt_yr The user-defined report year for the Seasonal Monitoring Report. Must be an integer.
#' @return Two ggplot layers that define the x-axis limits and breaks based upon user-defined arguments
#' @import ggplot2
#' @export
std_x_axis_rec_years <- function(rpt_yr) {
  # Define start year for recent years plots
  start_yr <- rpt_yr - 14

  # Define year breaks
  year_breaks <- seq(
    from = round_to_mult(start_yr, 5),
    to = round_to_mult(rpt_yr, 5),
    by = 5
  )

  # Create a list of layers to add to ggplot object
  list(
    scale_x_continuous(breaks = year_breaks),
    coord_cartesian(xlim = c(start_yr, rpt_yr))
  )
}

