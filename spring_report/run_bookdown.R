
library(bookdown)
library(png)
library(grid)

projectRoot <- "."
reportRoot <- file.path(projectRoot,"spring_report")

## Seems like bookdown works better if you change the working directory:
setwd(file.path(reportRoot,"report_bookdown"))
bookdown::render_book(input="index.Rmd", output_dir = "C:/Users/rhartman/OneDrive - California Department of Water Resources/status and trends/Status-and-Trends/docs")
bookdown::publish_book(account = "Rosemary_Hartman", render = 'local')

