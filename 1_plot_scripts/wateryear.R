#file to produce a graph of water year type and Sacramento Valley index over time for cover of report

source("setup.R")

library(tidyverse)
library(smonitr)


load(file.path(data_root,"wtryrs.RData"))

indicies = filter(water_year_indices, location != "San Joaquin Valley") 
in2022 = data.frame(WY = c( 2024, 2025, 2026), Index = c(8.44, 8.98, 7.81), 
                    Yr_type = c( "Above Normal", "Above Normal", "Above Normal"))
indicies = bind_rows(indicies, in2022)



row_num_new <- nrow(indicies) + 1
if(!(report_year %in% indicies$WY)) {
  indicies[row_num_new, ] <- rep(NA, ncol(indicies))
  indicies$WY[row_num_new] <- report_year
}

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
  geom_hline(yintercept = mean(indicies$Index), linetype =2, color = "red") +
  stat_missing(aes(x=WY, y=Index), size=2.5) + 
  smr_caption(stat_name="water year type", report_year=report_year)	+ 
  smr_alttext(stat_name="water year type")

water_year
ggsave("WType.tiff", device = "tiff", width =7, height =5)


getCaption(water_year)
getAlttext(water_year)

## Save figure:
save(water_year, 
     file=file.path(fig_root_landing,"water_year.RData"))
ggsave(file.path(fig_root_svg,"water_year.svg"), device = "svg", width =7, height =5)



