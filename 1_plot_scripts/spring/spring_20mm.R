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

# ## All years:
# dsm_allyears_fig <- ggplot(dsmIndexDf) + 
  # geom_bar(aes(x=Year_f, y=Index), stat="identity") +
  # theme_smr() + 
  # theme(legend.position="none") + 
  # scale_y_continuous("Index") + 
  # std_x_axis_all_years(rpt_yr=report_year, "discrete") +
  # lt_avg_line(lt_avg=dsm_lt_avg)

# ggsave(dsm_allyears_fig, file=file.path(fig_root_spring,"20mm_DSM_allyears.png"), 
       # dpi=300, units="cm", width=9.3, height=6.8)


# lfs_allyears_fig <- ggplot(lfsIndexDf) +
  # geom_bar(aes(x=Year_f, y=Index), stat="identity") + 
  # theme_smr() + 
  # theme(legend.position="none") + 
  # scale_y_continuous(expression(paste("Index"))) + 
  # std_x_axis_all_years(rpt_yr=report_year, "discrete") + 
  # lt_avg_line(lt_avg=lfs_lt_avg)

# ggsave(lfs_allyears_fig, file=file.path(fig_root_spring,"20mm_LFS_allyears.png"), 
       # dpi=300, units="cm", width=9.3, height=6.8)
	 
			 
## Recent years:
dsm_recyears_fig <- ggplot(dsmIndexDf)+
  geom_bar(aes(x=Year_f, y=Index), stat="identity") +
  theme_smr() + 
  theme(legend.position="none") + 
  scale_y_continuous("Index") + 
  xlab("Year") + 
  std_x_axis_rec_years(rpt_yr=report_year, "discrete") +
  lt_avg_line(lt_avg=dsm_lt_avg)

ggsave(dsm_recyears_fig, file=file.path(fig_root_spring,"20mm_DSM_recent.png"), 
       dpi=300, units="cm", width=9.3, height=6.8)


lfs_recyears_fig <- ggplot(lfsIndexDf)+
  geom_bar(aes(x=Year_f, y=Index), stat="identity") +
  theme_smr() + 
  theme(legend.position="none") + 
  scale_y_continuous("Index") + 
  xlab("Year") + 
  std_x_axis_rec_years(rpt_yr=report_year, "discrete") +
  lt_avg_line(lt_avg=lfs_lt_avg)

ggsave(lfs_recyears_fig, file=file.path(fig_root_spring,"20mm_LFS_recent.png"), 
       dpi=300, units="cm", width=9.3, height=6.8)

