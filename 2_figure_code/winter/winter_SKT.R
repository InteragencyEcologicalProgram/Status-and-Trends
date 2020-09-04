
library(tidyverse)

##########################################################################

skt_index_df <- read.csv(file.path(data_root,"SKT","skt_dsm_index.csv"))
skt_index_sub <- subset(skt_index_df, Year <= report_year)
skt_index_sub$fyear = as.factor(skt_index_sub$Year)

skt_dsm_fig <- ggplot(skt_index_sub, aes(x=Year, y=Index)) +
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  ylab("Delta Smelt Index") + 
  lt_avg_line(lt_avg=mean(skt_index_sub$Index, na.rm=TRUE)) + 
  std_x_axis_rec_years(rpt_yr=report_year, "cont") +
  #std_x_axis_label("winter") +       SKT index uses its own set of months
  missing_data_symb(skt_index_sub, yr_var=fyear, rpt_yr=report_year, symb_size=1)

ggsave(skt_dsm_fig, file="skt_dsm_fig.png", path=fig_root_winter, 
       dpi=300, units="cm", width=9.3, height=6.8)
