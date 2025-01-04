
# Bookdown --------------------------------------------------------------------------

source("setup.R")

## Seems like bookdown works better if you change the working directory:
setwd(bookdown_root)
render_book(input="index.Rmd", output_dir=file.path(root,"docs"))
#publish_book(account="Rosemary_Hartman", render="local")
#render_book(input="index.Rmd", output_format="all", output_dir=file.path(root,"report"))
setwd(root)

