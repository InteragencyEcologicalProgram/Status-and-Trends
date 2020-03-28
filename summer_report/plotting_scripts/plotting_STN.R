
report_year <- 2018

library(smonitr)
library(ggplot2)

projectRoot <- "."
reportRoot <- file.path(projectRoot,"summer_report")
dataRoot <- file.path(projectRoot,"data")
thisDataRoot <- file.path(dataRoot,"STN")
figRoot <- file.path(reportRoot,"figures")

localFile <- file.path(thisDataRoot,"STN_DSM_indices.csv")

##########################################################################

dsmIndexDf_raw <- read.csv(localFile, stringsAsFactors=FALSE)
dsmIndexDf_raw

## Truncate the data according to the specified report year:
dsmIndexDf <- subset(dsmIndexDf_raw, Year <= report_year)

##########################################################################
## Delta Smelt:

dsmIndexDf$fyear = as.factor(dsmIndexDf$Year)
dsmIndexDf = filter(dsmIndexDf, !is.na(Index))
dsm_fig <- ggplot(dsmIndexDf, aes(x=fyear, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  lt_avg_line(lt_avg=mean(dsmIndexDf$Index, na.rm=TRUE)) + 
  std_x_axis_all_years(rpt_yr=report_year, "discrete")+
  std_x_axis_label("summer") +
  missing_data_symb(dsmIndexDf, yr_var = fyear, rpt_yr = report_year, symb_size = 1)

dsm_fig
ggsave(dsm_fig, file=file.path(figRoot,"STN_DSM.png"), dpi=300, units="cm", 
			 width=9.3, height=6.8)

##recent years:

dsm_fig2 <- ggplot(dsmIndexDf, aes(x=fyear, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  lt_avg_line(lt_avg=mean(dsmIndexDf$Index, na.rm=TRUE)) + 
  std_x_axis_rec_years(rpt_yr=report_year, "discrete")+
  std_x_axis_label("summer") +
  missing_data_symb(dsmIndexDf, yr_var = fyear, rpt_yr = report_year, symb_size = 1)

dsm_fig2
ggsave(dsm_fig2, file=file.path(figRoot,"STN_DSM_rec.png"), dpi=300, units="cm", 
       width=9.3, height=6.8)

