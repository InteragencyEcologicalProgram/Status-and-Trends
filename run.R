
# Load libraries --------------------------------------------------------------------

library(smonitr)
library(tidyverse)
library(bookdown)
library(png)
library(grid)
library(svglite)


# Define global variables -----------------------------------------------------------

report_year <- 2020

root <- getwd()
data_access_root <- file.path(root,"0_data_access_scripts")
plot_root <- file.path(root,"1_plot_scripts")
bookdown_root <- file.path(root,"2_bookdown")
data_root <- file.path(root,"data")

fig_root <- file.path(root,"2_bookdown","figures")
fig_root_static <- file.path(fig_root,"static")
fig_root_fall <- file.path(fig_root,"fall")
fig_root_spring <- file.path(fig_root,"spring")
fig_root_summer <- file.path(fig_root,"summer")
fig_root_winter <- file.path(fig_root,"winter")

fig_root_svg <- file.path(fig_root,"svg")

source(file.path(plot_root,"util.R"))


# Download, (reduce), save, (delete) data -------------------------------------------

# source(file.path(data_access_root,"BayStudyFish_data_download.R"))
#source(file.path(data_access_root,"data_access_20mm.R"))
# source(file.path(data_access_root,"data_access_dayflow.R"))
#source(file.path(data_access_root,"data_access_DJFMP.R"))
# source(file.path(data_access_root,"data_access_fmwt.R"))
# source(file.path(data_access_root,"data_access_sacpass.R"))
# source(file.path(data_access_root,"data_access_SKT.R"))
# source(file.path(data_access_root,"data_access_STN.R"))
# source(file.path(data_access_root,"download_ybfmp.r"))
# source(file.path(data_access_root,"data_access_zoops.R"))
#source(file.path(data_access_root,"WQ_data_download.R"))


# Process and plot ------------------------------------------------------------------

#source(file.path(plot_root,"fall","IEP_Status&Trends_Fishes_Fall.R"))
# 
# source(file.path(plot_root,"spring","process_ybfmp_yolo_splittail_chinook.r"))
# source(file.path(plot_root,"spring","spring_20mm.R"))
#source(file.path(plot_root,"spring","spring_DJFMP.R"))
# 
# source(file.path(plot_root,"summer","BayStudyFishPlots_summer.R"))
# source(file.path(plot_root,"summer","IEP_Status&Trends_Microcystis.R"))
# source(file.path(plot_root,"summer","Summer_AquaticVegetation.R"))
# source(file.path(plot_root,"summer","summer_DJFMP.R"))
#source(file.path(plot_root,"summer","summer_STN.R"))
# 
# source(file.path(plot_root,"winter","BayStudyFishPlots.R"))
# source(file.path(plot_root,"winter","winter_DJFMP.R"))
# source(file.path(plot_root,"winter","winter_SKT.R"))
# 
# source(file.path(plot_root,"Graphs_Sacpass.R"))
# source(file.path(plot_root,"IEP_Status&Trends_Flow.R"))
#source(file.path(plot_root,"IEP_Status&Trends_WaterQuality.R"))
# source(file.path(plot_root,"IEP_Status&Trends_Zooplankton.R"))
# source(file.path(plot_root,"mline.R"))
  

# Bookdown --------------------------------------------------------------------------

## Seems like bookdown works better if you change the working directory:
setwd(bookdown_root)
render_book(input="index.Rmd", output_dir=file.path(root,"docs"))
#publish_book(account="Rosemary_Hartman", render="local")
#render_book(input="index.Rmd", output_format="all", output_dir=file.path(root,"report"))
setwd(root)

