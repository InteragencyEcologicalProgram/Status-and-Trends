## Spring = March, April, May
## See a copy of the document "#22 Metadata (Updated May 30, 2019).doc" for reference.

##########################################################################
## Read in data:

#thisDataRoot <- file.path(data_root,"DJFMP")

#chippsData <- read.csv(file.path(thisDataRoot,"chippsData.csv"), stringsAsFactors=FALSE)
load(file.path(data_root,"chippsData.RData"))

##########################################################################
## Chipps Trawl: Winterrun Chinook

## Fill in missing volumes with an overall average:
any(is.na(unique(chippsData$Volume)))
chippsData$Volume[is.na(chippsData$Volume)] <- mean(chippsData$Volume, na.rm=TRUE)

## Add fields:
chippsData$SampleDate = as.Date(chippsData$SampleDate, "%m/%d/%Y")
chippsData$Month <- lubridate::month(chippsData$SampleDate)
chippsData$Year <- lubridate::year(chippsData$SampleDate)
chippsData$Year_f <- as.factor(chippsData$Year)

## Create wide data frame:
chippsData$CommonName <- sub("Chinook salmon","Chinook salmon_",chippsData$CommonName)
chippsData$CommonName <- sub(" ","_",chippsData$CommonName)
chippsData$RaceByLength[is.na(chippsData$RaceByLength)] <- ""
chippsData$CommonName_RaceByLength <- with(chippsData, paste0(CommonName, RaceByLength))
keep_fields_chipps <- c("Year","Year_f","Month","Location","RegionCode","StationCode",
												"SampleDate","SampleTime","MethodCode","GearConditionCode",
												"TowNumber","Volume","CommonName_RaceByLength","Catch")
chippsWide <- tidyr::spread(chippsData[ ,keep_fields_chipps], CommonName_RaceByLength, 
															Catch, fill=0)
chippsWide$"No catch" <- NULL


## Truncate the data according to the specified report year and season:
chippsWide_spring <- subset(chippsWide, 1995 <= Year & Year <= report_year & 
                                        Month %in% 3:5)

## Calculate indices:
chippsIndexDf <- chippsWide_spring %>%
	dplyr::group_by(Year_f, Year, Month) %>%
	dplyr::summarize(
		chinook_winterByLength_CPUE_YM=sum(Chinook_salmon_Winter/Volume),
		.groups="keep"
	) %>% 
	dplyr::ungroup() %>%
	dplyr::group_by(Year_f, Year) %>%
	dplyr::summarize(
		chinook_winterByLengthIndex=mean(chinook_winterByLength_CPUE_YM) * 1000,
		.groups="keep"
	) %>% 
	dplyr::ungroup() %>%
	as.data.frame(.)
chippsIndexDf


##########################################################################
## Figures:

use_ylab <- "Index for unmarked fish"
chinook_lt_avg <- mean(chippsIndexDf$chinook_winterByLengthIndex, na.rm=TRUE)

## All years:
chinook_winterByLength_allYears_fig <- ggplot(chippsIndexDf, 
		aes(x=Year, y=chinook_winterByLengthIndex)) + 
  geom_bar(stat="identity") +
  smr_theme_update() + 
  smr_x_axis(report_year, "all", "spring") + 
  ylab(use_ylab)+
	#stat_missing(size=2.5) + 
  stat_lt_avg() + 
  annotate("text", x=1968, y=1, label="Earlier data\nomitted", 
           hjust=0, size=2.7) + 	
	smr_caption(stat_name="the juvenile winter-run Chinook salmon index", 
							report_year=report_year) + 
	smr_alttext(stat_name="juvenile winter-run Chinook index")

chinook_winterByLength_allYears_fig

getCaption(chinook_winterByLength_allYears_fig)
getAlttext(chinook_winterByLength_allYears_fig)


save(list=c("chinook_winterByLength_allYears_fig"), 
		 file=file.path(fig_root_spring,"DJFMP_chinook_winterByLength_allyears_SP.RData"))


## Recent years:
chinook_winterByLength_recYears_fig <- ggplot(chippsIndexDf,
		aes(x=Year, y=chinook_winterByLengthIndex)) + 
  geom_bar(stat="identity") + 
  smr_theme_update() + 
  smr_x_axis(report_year, "recent", "spring") + 
  ylab(use_ylab)+
	stat_missing(size=2.5) + 
  stat_lt_avg() + 
	smr_caption(stat_name="the juvenile winter-run Chinook salmon index", 
							report_year=report_year) + 
	smr_alttext(stat_name="juvenile winter-run Chinook index")

chinook_winterByLength_recYears_fig

getCaption(chinook_winterByLength_recYears_fig)
getAlttext(chinook_winterByLength_recYears_fig)


save(list=c("chinook_winterByLength_recYears_fig"), 
		 file=file.path(fig_root_spring,"DJFMP_chinook_winterByLength_recyears_SP.RData"))

