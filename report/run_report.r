
library(gridExtra)
library(knitr)

## Update root:
root <- file.path("...","Status-and-Trends")

## Makes life easier to have the report directory as the working directory:
setwd(file.path(root,"report"))

source(file.path(root,"IEP_Status&Trends_WaterQuality.R"))
source(file.path(root,"IEP_Status&Trends_Flow.R"))
source(file.path(root,"IEP_Status&Trends_Zooplankton.R"))
source(file.path(root,"IEP_Status&Trends_Fishes_Fall.R"))


## knitr pdf version:
base <- "status_and_trends_report_fall"
knitr::knit(input=paste0(base,".Rnw"))
tools::texi2dvi(paste0(base,".tex"), pdf=TRUE, clean=TRUE)
