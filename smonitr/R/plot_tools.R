#' @title Custom formatting for ggplots in the IEP Seasonal Monitoring Reports
#' @description This custom theme builds off of the \code{theme_bw()}
#'     ggplot theme and removes plot gridlines and modifies the text size of
#'     various ggplot elements.
#'
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
#'     is to represent the provided long-term average of a dataset. For now the
#'     user is required to input the long-term average value into the function.
#'
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
#' @description Used to round a year value to the nearest multiple of a defined
#'     value. This is an internal function used to define x-axis breaks for plots.
#'
#' @param num Integer to round
#' @param mult Mulitple value to round to
#' @keywords internal
#' @return A value rounded to the nearest multiple of the \code{mult} parameter
round_to_mult <- function(num, mult) {
  num_round <- round(num/mult) * mult
  return(num_round)
}


#' @title  Standardize x-axis elements for ggplot of all years
#' @description Standardizes the x-axis limits and breaks for a ggplot of
#'     all years of data.
#'
#' @section Special Instructions:
#' The x-variable in the ggplot needs to be "numeric" type.
#'
#' @param rpt_yr The user-defined report year for the Seasonal Monitoring Report.
#'     Must be an integer.
#' @param start_yr The start year that defines the minimum x-axis limit for the
#'     ggplot. Must be an integer. Default is 1966.
#' @return Two ggplot layers that define the x-axis limits and breaks based upon
#'     user-defined arguments
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
#' @description Standardizes the x-axis limits and breaks for a ggplot of recent
#'     years of data. "Recent years" is defined as the prior 15 years from the
#'     \code{rpt_yr}.
#'
#' @section Special Instructions:
#' The x-variable in the ggplot needs to be "numeric" type. This function
#'     works best if the dataset is filtered to the necessary date range
#'     beforehand.
#'
#' @param rpt_yr The user-defined report year for the Seasonal Monitoring Report.
#'     Must be an integer.
#' @return Two ggplot layers that define the x-axis limits and breaks based upon
#'     user-defined arguments
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


#' @title Add symbols to a ggplot representing missing data
#' @description Adds tan triangle symbols to a ggplot to represent years with
#'     missing data. This function is intended to be used on plots with
#'     \code{geom_col} geoms.
#'
#' @param df The dataframe that contains the data in the ggplot
#' @param yr_var The name of the variable in \code{df} that contains the years
#'     used in the ggplot. Must be "numeric" type.
#' @param rpt_yr The user-defined report year for the Seasonal Monitoring Report.
#'     Must be an integer.
#' @param symb_size Specifies the size of the symbol used to represent missing
#'     data.
#'
#' @return A ggplot object with tan triangle symbols that represent years with
#'     missing data.
#' @import ggplot2
#' @importFrom rlang enquo
#' @importFrom rlang as_name
#' @importFrom dplyr pull
#' @importFrom dplyr anti_join
#' @importFrom tibble tibble
#' @export
missing_data_symb <- function(df, yr_var, rpt_yr, symb_size) {
  # Convert yr_var to enquo for non-standard eval
  yr_var_enquo <- enquo(yr_var)

  # Calculate minimum year in df
  yr_min <- min(pull(df, !!yr_var_enquo))

  # Create a tibble with all possible years
  all_yrs <- tibble(
    years = seq(yr_min, rpt_yr, 1),
    result = 0
  )

  # Find missing years in df
  missing_yrs <- anti_join(
    all_yrs,
    df,
    by = c("years" = as_name(yr_var_enquo))
  )

  # Add in symbols for missing years if necessary
  if (!is.null(missing_yrs)) {
    geom_point(
      data = missing_yrs,
      aes(
        x = years,
        y = result
      ),
      inherit.aes = FALSE,
      na.rm = TRUE,
      shape = 24,
      size = symb_size,
      fill = "tan2",
      color = "gray10"
    )
  }

}


# Next steps --------------------------------------------------------------

# 1. Possibly modify the lt_avg_line function to be able to receive the
# dataframe and name of the values variable to calculate the average
# within the function itself.
