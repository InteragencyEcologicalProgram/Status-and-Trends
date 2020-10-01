
library(bookdown)
library(png)
library(grid)

projectRoot <- "."

## Seems like bookdown works better if you change the working directory:
setwd(file.path(projectRoot,"report_bookdown"))
bookdown::render_book(input="index.Rmd", 
                      output_dir =file.path("..","docs"))
bookdown::publish_book(account = "Rosemary_Hartman", render = 'local')

bookdown::render_book(input="index.Rmd", output_format = "all",
                      output_dir =file.path("..","docs"))
