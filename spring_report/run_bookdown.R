
library(bookdown)
library(png)
library(grid)

projectRoot <- "."
reportRoot <- file.path(projectRoot,"spring_report")

## Seems like bookdown works better if you change the working directory:
setwd(file.path(reportRoot,"report_bookdown"))
bookdown::render_book(input="index.Rmd")

