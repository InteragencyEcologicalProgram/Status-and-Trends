
# Load libraries --------------------------------------------------------------------

library(smonitr)
library(tidyverse)
library(bookdown)
library(png)
library(grid)
library(svglite)


# Define global variables -----------------------------------------------------------

report_year <- 2021

root <- getwd()
data_access_root <- file.path(root,"0_data_access_scripts")
plot_root <- file.path(root,"1_plot_scripts")
bookdown_root <- file.path(root,"2_bookdown")
data_root <- file.path(root,"data")

fig_root <- file.path(root,"2_bookdown","figures")
fig_root_static <- file.path(fig_root,"static")
fig_root_landing <- file.path(fig_root,"landing_page")
fig_root_fall <- file.path(fig_root,"fall")
fig_root_spring <- file.path(fig_root,"spring")
fig_root_summer <- file.path(fig_root,"summer")
fig_root_winter <- file.path(fig_root,"winter")

fig_root_svg <- file.path(fig_root,"svg")

source(file.path(plot_root,"util.R"))

