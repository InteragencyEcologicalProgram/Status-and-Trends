#' @title Custom formatting for ggplots in the IEP Seasonal Monitoring Reports
#' @description This custom theme builds off of the \code{theme_bw()}
#'     ggplot theme and removes plot gridlines and modifies the text size of
#'     various ggplot elements.
#'
#' @return A ggplot layer that applies a custom theme as described above.
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
    legend.title = element_text(size = 10),
    # Define legend position
    legend.position = "top"
  )
}


#' @title Add a horizontal line to a ggplot
#' @description Adds a horizontal dashed red line to a ggplot. Its intended use
#'     is to represent the provided long-term average of a dataset. For now the
#'     user is required to input the long-term average value into the function.
#'
#' @param lt_avg The long-term average of the data
#'
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
#'
#' @return A value rounded to the nearest multiple of the \code{mult} parameter
round_to_mult <- function(num, mult) {
  num_round <- round(num/mult) * mult
  return(num_round)
}


#' @title  Standardize x-axis elements for ggplot of all years
#' @description Standardizes the x-axis limits and breaks for a ggplot of
#'     all years of data. For plots with a discrete x-variable including
#'     \code{geom_col} geoms, the x-variable used in the ggplot needs to
#'     be a \strong{factor}.
#'
#' @param rpt_yr The user-defined report year for the Seasonal Monitoring Report.
#'     Must be an integer.
#' @param x_scale_type The scale type for the x variable. Can be either
#'     \code{x_scale_type = "discrete"} or \code{x_scale_type = "cont"}. Plots
#'     with \code{geom_col} geoms are "discrete" scale type, and \code{geom_line}
#'     and \code{geom_area} geoms are "cont" or continuous scale type.
#' @param start_yr The start year that defines the minimum x-axis limit for the
#'     ggplot. Must be an integer. Default is 1966.
#' @param break_int The x-axis break interval in years. Must be an integer.
#'     Default is 10.
#'
#' @return A ggplot layer that defines the x-axis limits and breaks based upon
#'     user-defined arguments.
#' @import ggplot2
#' @export
std_x_axis_all_years <- function(rpt_yr,
                                 x_scale_type = c("discrete", "cont"),
                                 start_yr = 1966,
                                 break_int = 10) {
  # Evaluate choices for scale_type
  x_scale_type <- match.arg(x_scale_type, c("discrete", "cont"))

  # Define year breaks
  year_breaks <- seq(
    from = round_to_mult(start_yr, break_int),
    to = round_to_mult(rpt_yr, break_int),
    by = break_int
  )

  # Add layer to ggplot object
  if (x_scale_type == "discrete") {
    scale_x_discrete(
      limits = as.character(start_yr:rpt_yr),
      breaks = as.character(year_breaks)
    )
  } else {
    scale_x_continuous(
      limits = c(start_yr, rpt_yr),
      breaks = year_breaks
    )
  }
}


#' @title  Standardize x-axis elements for ggplot of recent years
#' @description Standardizes the x-axis limits and breaks for a ggplot of recent
#'     years of data. "Recent years" is defined as the prior 15 years from the
#'     \code{rpt_yr} argument. For plots with \code{geom_col} geoms, the
#'     x-variable used in the ggplot needs to be a \strong{factor}.
#'
#' @param rpt_yr The user-defined report year for the Seasonal Monitoring Report.
#'     Must be an integer.
#' @param x_scale_type The scale type for the x variable. Can be either
#'     \code{x_scale_type = "discrete"} or \code{x_scale_type = "cont"}. Plots
#'     with \code{geom_col} geoms are "discrete" scale type, and \code{geom_line}
#'     and \code{geom_area} geoms are "cont" or continuous scale type. Default is
#'     \code{x_scale_type = "discrete"}.
#'
#' @return A ggplot layer that defines the x-axis limits and breaks for plots of
#'     recent data. The default limits are \code{rpt_yr - 14} to \code{rpt_yr},
#'     and the default break interval is 5 years.
#' @import ggplot2
#' @export
std_x_axis_rec_years <- function(rpt_yr, x_scale_type = c("discrete", "cont")) {
  std_x_axis_all_years(rpt_yr, x_scale_type, start_yr = rpt_yr - 14, break_int = 5)
}


#' @title Add a standardized x-axis label to a ggplot
#' @description Adds a standardized x-axis label to a ggplot based on the season
#'     of the report.
#'
#' @param season The user defined season for the Seasonal Monitoring Report. Must be
#'     one of the following: \code{"winter", "spring", "summer", "fall", "annual"}.
#'
#' @return A standardized x-axis label to a ggplot object. Each season option
#'     provides a label in the format "Year (Month - Month)". The argument
#'     \code{season = "annual"} provides the label "Year" without specified months.
#' @importFrom dplyr case_when
#' @import ggplot2
#' @export
std_x_axis_label <- function(season = c("winter", "spring", "summer", "fall", "annual")) {
  # Evaluate choices for season arg
  season <- match.arg(season, c("winter", "spring", "summer", "fall", "annual"))

  # Define x-axis label depending on the season arg
  x_axis_label <- case_when(
    season == "winter" ~ "Year (December - February)",
    season == "spring" ~ "Year (March - May)",
    season == "summer" ~ "Year (June - August)",
    season == "fall" ~ "Year (September - November)",
    TRUE ~ "Year"
  )

  # Add layer to ggplot object
  xlab(x_axis_label)
}

#' @title Add symbols to a ggplot representing missing data
#' @description Adds tan triangle symbols to a ggplot to represent years with
#'     missing data. This function is intended to be used on plots with
#'     \code{geom_col} geoms.
#'
#' @param df The dataframe that contains the data in the ggplot
#' @param yr_var The name of the variable in \code{df} that contains the years
#'     used in the ggplot. Should be a \strong{factor}.
#' @param rpt_yr The user-defined report year for the Seasonal Monitoring Report.
#'     Must be an integer.
#' @param symb_size Specifies the size of the symbol used to represent missing
#'     data. Must be numeric. Usually either \code{symb_size = 1} or
#'     \code{symb_size = 2}.
#'
#' @return A ggplot object with tan triangle symbols that represent years with
#'     missing data.
#' @import ggplot2
#' @importFrom rlang enquo
#' @importFrom rlang as_name
#' @importFrom dplyr pull
#' @importFrom dplyr anti_join
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @export
missing_data_symb <- function(df, yr_var, rpt_yr, symb_size) {
  # Convert yr_var to enquo for non-standard eval
  yr_var_enquo <- enquo(yr_var)

  # Convert yr_var in df to numeric class
  df1 <- df %>%
    mutate(!!yr_var_enquo := as.numeric(as.character(!!yr_var_enquo)))

  # Calculate minimum year in df
  yr_min <- min(pull(df1, !!yr_var_enquo))

  # Create a tibble with all possible years
  all_yrs <- tibble(
    years = seq.int(yr_min, rpt_yr, 1),
    result = 0
  )

  # Find missing years in df
  missing_yrs <-
    anti_join(
      all_yrs,
      df1,
      by = c("years" = as_name(yr_var_enquo))
    ) %>%
    mutate(years = factor(years))

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
