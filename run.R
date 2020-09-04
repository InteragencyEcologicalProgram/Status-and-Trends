
library(smonitr)
library(bookdown)
library(png)
library(grid)


report_year <- 2018


data_access_root <- file.path(getwd(),"0_data_access_scripts")

data_root <- file.path(getwd(),"1_data")

fig_root <- file.path(getwd(),"3_bookdown","figures")
fig_root_static <- file.path(fig_root,"static")
fig_root_fall <- file.path(fig_root,"fall")
fig_root_spring <- file.path(fig_root,"spring")
fig_root_summer <- file.path(fig_root,"summer")
fig_root_winter <- file.path(fig_root,"winter")


# Bookdown --------------------------------------------------------------------------

## Seems like bookdown works better if you change the working directory:
root <- getwd()
setwd("3_bookdown")
render_book(input="index.Rmd",  output_dir =file.path("..","docs"))
publish_book(account="Rosemary_Hartman", render="local")
render_book(input="index.Rmd", output_format="all", output_dir=file.path(root,"docs"))
setwd(root)
