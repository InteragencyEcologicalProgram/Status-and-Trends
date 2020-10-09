## Summer

##########################################################################
## Read in data:

thisDataRoot <- file.path(data_root,"STN")

stnDSMIndex <- read.csv(file.path(thisDataRoot,"STN_DSM_indices.csv"), 
												stringsAsFactors=FALSE)


##########################################################################
## Summer Townet Survey: Delta Smelt

## Add fields:
stnDSMIndex$Year_f = as.factor(stnDSMIndex$Year)

## Truncate the data according to the specified report year:
stnDSMIndex <- subset(stnDSMIndex, !is.na(Index))
stnDSMIndex <- subset(stnDSMIndex, Year <= report_year)

##########################################################################
## Figures:

dsm_lt_avg <- mean(stnDSMIndex$Index, na.rm=TRUE)

## All years:
stn_dsm_allyears_fig <- ggplot(stnDSMIndex, aes(x=Year_f, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  std_x_axis_all_years(rpt_yr=report_year, "discrete") +
  std_x_axis_label("summer") +
  missing_data_symb(df=stnDSMIndex, yr_var=Year_f, rpt_yr=report_year, symb_size=1) + 
  lt_avg_line(lt_avg=dsm_lt_avg)

ggsave(stn_dsm_allyears_fig, file=file.path(fig_root_summer,"STN_DSM.png"), 
			 dpi=300, units="cm", width=9.3, height=6.8)


## Recent years:
stn_dsm_recyears_fig <- ggplot(stnDSMIndex)+
  geom_bar(aes(x=Year_f, y=Index), stat="identity") +
  theme_smr() + 
	# Update theme_smr's plot margins so this plot better matches the Microcystis 
	# and aquatic vegetation plots:
	theme(plot.margin = unit(c(0.25+0.7, 0.6, 0.1, 0.4), units="cm")) + 
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  std_x_axis_rec_years(rpt_yr=report_year, "discrete") +
  std_x_axis_label("summer") +
  missing_data_symb(df=stnDSMIndex, yr_var=Year_f, rpt_yr=report_year, symb_size=1) + 
  lt_avg_line(lt_avg=dsm_lt_avg)

ggsave(stn_dsm_recyears_fig, file=file.path(fig_root_summer,"STN_DSM_rec.png"), 
       dpi=300, units="cm", width=9.3, height=7.5)   # width=9.3, height=6.8)

