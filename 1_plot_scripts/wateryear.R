#file to produce a graph of water year type and Sacramento Valley index over time for cover of report

library(waterYearType)
library(tidyverse)
library(smonitr)


url <- "https://github.com/FlowWest/waterYearType/blob/master/data/water_year_indices.rda?raw=true"

download.file(url, destfile= "./wtryrs.RData", mode = "wb")
load("./wtryrs.RData")

indicies = filter(water_year_indices, location != "San Joaquin Valley")

water_year = ggplot(indicies, aes(x=WY, y=Index)) +
  geom_bar(stat="identity", aes(fill=Yr_type)) + 
  scale_fill_manual(values = c("Critical"="firebrick",
                               "Dry"="firebrick1",  
                               "Below Normal"="darkorange", 
                               "Above Normal"="chartreuse3",
                               "Wet"="dodgerblue")) +
  smr_theme_update() + 
  smr_x_axis(report_year, type="all", season="annual") +
  ylab("Sacramento River 8-station Index") +
  xlab("Water Year") +
  labs(fill="Year Type") + 
  #coord_cartesian(xlim = c(1975, 2020)) +
  stat_lt_avg() +
  smr_caption(stat_name="water year type", report_year=report_year)	+ 
  smr_alttext(stat_name="water year type")

water_year

getCaption(water_year)
getAlttext(water_year)

## Save figure:
save(list="water_year", 
     file=file.path(fig_root_landing,"water_year.RData"))


