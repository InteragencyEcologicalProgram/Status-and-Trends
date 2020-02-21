#plot spring-run adult salmon

library(tidyverse)
library(smonitr)
source("data_access_scripts/data_access_sacpass.R")


#Spring run salmon

p_spch <- ggplot(Spring, aes(x=Year, y=sprinrun))+
    geom_bar(stat="identity", fill="salmon1") +
    theme_smr() +
    theme(legend.position="none") + 
    scale_y_continuous("Spring Run Chinook Adult Returns",limits=c(0, max(Spring$sprinrun))) +
    std_x_axis_all_years(2018) +
  lt_avg_line(mean(Spring$sprinrun))
p_spch

ggsave(p_spch, file="SpringRun_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "spring_report/figures")


#Fall run salmon

p_frch <- ggplot(Fall, aes(x=Year, y=FallRun))+
  geom_bar(stat="identity", fill="salmon1") +
  theme_smr() +
  theme(legend.position="none") + 
  scale_y_continuous("Fall Run Chinook Adult Returns",limits=c(0, max(Fall$FallRun))) +
  std_x_axis_all_years(2018) +
  lt_avg_line(mean(Fall$FallRun))
p_frch

ggsave(p_frch, file="FallRun_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")


