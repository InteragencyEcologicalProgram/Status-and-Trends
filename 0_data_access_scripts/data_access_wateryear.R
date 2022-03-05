#file to access water year type and Sacramento Valley index over time for cover of report

source("setup.R")

library(waterYearType)
library(tidyverse)
library(smonitr)


url <- "https://github.com/FlowWest/waterYearType/blob/master/data/water_year_indices.rda?raw=true"

download.file(url, destfile=file.path(data_root,"wtryrs.RData"), mode = "wb")

