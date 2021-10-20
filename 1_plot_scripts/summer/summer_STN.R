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

## All years:
stn_dsm_all_years <- ggplot(stnDSMIndex, aes(x=Year, y=Index)) + 
  geom_bar(stat="identity") + 
  smr_theme_update() + 
  scale_y_continuous(expression(paste("Index"))) + 
  smr_x_axis(report_year, type = "all", season = "summer") + 
	stat_lt_avg() + 
  stat_missing(size=2.5) + 
	smr_caption(stat_name="the Delta Smelt index", report_year=report_year) + 
	smr_alttext(stat_name="juvenile Delta Smelt index")

stn_dsm_all_years

getCaption(stn_dsm_all_years)
getAlttext(stn_dsm_all_years)


## Recent years:
stn_dsm_recent_years <- ggplot(stnDSMIndex, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
	smr_theme_update() +
  scale_y_continuous(expression(paste("Index"))) + 
  smr_x_axis(report_year, type = "recent", season = "summer") + 
  stat_lt_avg() +
  stat_missing(size=2.5) + 
	smr_caption(stat_name="the Delta Smelt index", report_year=report_year) + 
	smr_alttext(stat_name="juvenile Delta Smelt index")

stn_dsm_recent_years

getCaption(stn_dsm_recent_years)
getAlttext(stn_dsm_recent_years)


## Save plots:
STN_delta_smelt <- list()
STN_delta_smelt[["all_years"]] <- stn_dsm_all_years
STN_delta_smelt[["recent_years"]] <- stn_dsm_recent_years

save(list="STN_delta_smelt", file=file.path(fig_root_summer,"STN_delta_smelt.RData"))

