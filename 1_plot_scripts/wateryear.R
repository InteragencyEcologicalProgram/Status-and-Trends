#file to produce a graph of water year type and Sacramento Valley index over time for cover of report

library(waterYearType)
library(tidyverse)
library(smonitr)


url <- "https://github.com/FlowWest/waterYearType/blob/master/data/water_year_indices.rda?raw=true"

download.file(url, destfile= "./wtryrs.RData", mode = "wb")
load("./wtryrs.RData")

indecies = filter(water_year_indices, location != "San Joaquin Valley")

yrtyps = ggplot(filter(indecies, location != "San Joaquin Valley"), aes(x = WY, y = Index))+
  geom_bar(stat = "identity", aes(fill = Yr_type))+  
  scale_fill_manual(values = c("chartreuse3", "darkorange", "firebrick", "firebrick1", "dodgerblue"))+
ylab("Sacramento River 8-station Index")+
  xlab("Water Year")+
  coord_cartesian(xlim = c(1975, 2020))+
  smr_theme()+
  smr_caption(stat_name="Water Year Types", report_year=reportyear)	+ 
  smr_alttext(stat_name="Water Year Types")

yrtyps



