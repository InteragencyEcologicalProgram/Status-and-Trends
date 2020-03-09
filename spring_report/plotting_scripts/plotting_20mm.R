
library(ggplot2)

report_year <- 2018

library(odbc)
library(DBI)
library(tidyverse)
library(smonitr)

projectRoot <- "."
reportRoot <- file.path(projectRoot,"spring_report")
dataRoot <- file.path(projectRoot,"data")
thisDataRoot <- file.path(dataRoot,"20mm")
figRoot <- file.path(reportRoot,"figures")

source(file.path(projectRoot,"smonitr","R","plot_tools.R"))

##########################################################################
## Read in index data:

dsmIndexDf_raw <- read.csv(file.path(thisDataRoot,"20mm_DSM_index.csv"),
                           stringsAsFactors=FALSE)
lfsIndexDf_raw <- read.csv(file.path(thisDataRoot,"20mm_LFS_index.csv"), 
                           stringsAsFactors=FALSE)

## Truncate the data according to the specified report year:
dsmIndexDf <- subset(dsmIndexDf_raw, Year <= report_year)
lfsIndexDf <- subset(lfsIndexDf_raw, Year <= report_year)

##########################################################################
## Delta Smelt

dsm_fig <- ggplot(dsmIndexDf, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() + 
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  lt_avg_line(lt_avg=mean(dsmIndexDf$Index, na.rm=TRUE)) + 
  std_x_axis_all_years(rpt_yr=report_year, start_yr=min(dsmIndexDf$Year), "cont")+
  std_x_axis_label("spring")

ggsave(dsm_fig, file=file.path(figRoot,"20mm_DSM.png"), dpi=300, units="cm", 
			 width=9.3, height=6.8)

#recent years only
dsmIndexDf$fyear = as.factor(dsmIndexDf$Year)
dsm_fig2 <- ggplot(dsmIndexDf, aes(x=fyear, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() + 
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  lt_avg_line(lt_avg=mean(dsmIndexDf$Index, na.rm=TRUE)) + 
  std_x_axis_rec_years(rpt_yr=report_year, "discrete") +
  std_x_axis_label("spring")

ggsave(dsm_fig2, file=file.path(figRoot,"20mm_DSM_recent.png"), dpi=300, units="cm", 
       width=9.3, height=6.8)

			 
##########################################################################
## Longfin Smelt

lfs_fig <- ggplot(filter(lfsIndexDf, Year <=2018), aes(x=Year, y=Index))+
  geom_bar(stat="identity") +

  theme_smr() + 
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  lt_avg_line(lt_avg=mean(dsmIndexDf$Index, na.rm=TRUE)) + 
  std_x_axis_all_years(rpt_yr=report_year, start_yr=min(lfsIndexDf$Year))

ggsave(lfs_fig, file=file.path(figRoot,"20mm_LFS.png"), dpi=300, units="cm", 
			 width=9.3, height=6.8)

library(lubridate)

dsm_fig +coord_cartesian(xlim = c(ymd_hms("2019-02-03 00:00:00"), ymd_hms("2019-02-06 00:00:00")))

limits =c(ymd_hms("2019-02-03 00:00:00"), ymd_hms("2019-02-06 00:00:00"))

#recent years only
lfsIndexDf$fyear = as.factor(lfsIndexDf$Year)
lfs_fig2 <- ggplot(filter(lfsIndexDf, Year <=2018), aes(x=fyear, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() + 
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  lt_avg_line(lt_avg=mean(lfsIndexDf$Index, na.rm=TRUE)) + 
  std_x_axis_rec_years(rpt_yr=report_year, "discrete") +
  std_x_axis_label("spring")

ggsave(lfs_fig2, file=file.path(figRoot,"20mm_LFS_recent.png"), dpi=300, units="cm", 
       width=9.3, height=6.8)


