## Spring

##########################################################################
## Read in data:

thisDataRoot <- file.path(data_root,"20mm")

dsmIndexDf <- read.csv(file.path(thisDataRoot,"20mm_DSM_index.csv"),
                       stringsAsFactors=FALSE)
lfsIndexDf <- read.csv(file.path(thisDataRoot,"20mm_LFS_index.csv"), 
                       stringsAsFactors=FALSE)

##########################################################################
## 20mm Survey: Delta Smelt and Longfin Smelt

## Add fields:
dsmIndexDf$Year_f <- as.factor(dsmIndexDf$Year)
lfsIndexDf$Year_f <- as.factor(lfsIndexDf$Year)

## Truncate the data according to the specified report year:
dsmIndexDf <- subset(dsmIndexDf, Year <= report_year)
lfsIndexDf <- subset(lfsIndexDf, Year <= report_year)

##########################################################################
## Figures:

dsm_lt_avg <- mean(dsmIndexDf$Index, na.rm=TRUE)
lfs_lt_avg <- mean(lfsIndexDf$Index, na.rm=TRUE)


## Recent years:
dsm_recyears_fig <- ggplot(dsmIndexDf, aes(x=Year, y=Index)) +
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("Index") + 
  smr_x_axis(report_year, type = "recent", season = "spring") +
	stat_missing(size=2.5) + 
  stat_lt_avg() + 
	smr_caption(stat_name="the Delta Smelt index", report_year=report_year) + 
	smr_alttext(stat_name="post-larval Delta Smelt index")

dsm_recyears_fig

getCaption(dsm_recyears_fig)
getAlttext(dsm_recyears_fig)

save(dsm_recyears_fig, file=file.path(fig_root_spring,"20mm_DSM_recent.RData"))


lfs_recyears_fig <- ggplot(lfsIndexDf, aes(x=Year, y=Index)) +
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("Index") + 
  smr_x_axis(report_year, type = "recent", season = "spring") +
	stat_missing(size=2.5) + 	
  stat_lt_avg() + 
	smr_caption(stat_name="the Longfin Smelt index", report_year=report_year) + 
	smr_alttext(stat_name="Longfin Smelt index")

lfs_recyears_fig

getCaption(lfs_recyears_fig)
getAlttext(lfs_recyears_fig)

save(list=c("lfs_recyears_fig"), file=file.path(fig_root_spring,"20mm_LFS_recent.RData"))

