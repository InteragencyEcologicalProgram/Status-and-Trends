## Summer = June, July, August

source("setup.R")

##########################################################################
## Read in data:

load("data/seineData.RData")

##########################################################################
## Beach Seine: Sacramento Pikeminnow

## Fill in missing volumes with an overall average:
unique(seineData$Volume)
seineData$Volume[is.na(seineData$Volume)] <- mean(seineData$Volume, na.rm=TRUE)

## Add fields:
seineData$Month <- lubridate::month(seineData$SampleDate)
seineData$Year <- lubridate::year(seineData$SampleDate)
seineData$Year_f <- as.factor(seineData$Year)

## Add lat/long:
seineData <- dplyr::left_join(
	seineData, 
	siteLatLong[ ,c("StationCode","Latitude","Longitude")],
	by="StationCode"
)

## These region designations came from Nick's code:
seineData$customRegion <- ""
seineData$customRegion[seineData$Longitude < -122.216] <- "San Pablo Bay"
seineData$customRegion[seineData$Longitude > -122.216 & 
											 seineData$Longitude < -121.829] <- "Suisun"
seineData$customRegion[seineData$Longitude > -121.829] <- "The Delta"
unique(seineData$customRegion)

## Create wide data frame:
seineData$CommonName <- sub("Chinook salmon","Chinook salmon_",seineData$CommonName)
seineData$RaceByLength[is.na(seineData$RaceByLength)] <- ""
seineData$CommonName_RaceByLength <- with(seineData, paste0(CommonName, RaceByLength))
keep_fields_seine <- c("Year","Month","Location","RegionCode","StationCode","SampleDate",
											 "SampleTime","MethodCode","GearConditionCode","TowNumber",
											 "Volume","CommonName_RaceByLength","Catch")
seineWide <- tidyr::spread(seineData[ ,keep_fields_seine], CommonName_RaceByLength, 
													 Catch, fill=0)
seineWide$"No catch" <- NULL
names(seineWide) <- sub(" ", "_", names(seineWide))

head(seineWide)
lapply(split(seineWide$StationCode, seineWide$Year), unique)


## Truncate the data according to the specified report year.
## Maybe keep 1995+ as a lazy way of dealing with sampling site inconsistencies.
seineWide_summer <- subset(seineWide, 1995 <= Year & Year <= report_year & Month %in% 6:8)

## Exclude DJFMP's region 1, which is north of Sacramento:
seineWide_summer <- subset(seineWide_summer, RegionCode != 1)

## Calculate indices:
seineIndexDf <- seineWide_summer %>%
	dplyr::group_by(Year, Month) %>%
	dplyr::summarize(
		SacPikeminnow_CPUE_YM=sum(Sacramento_pikeminnow/Volume),
		.groups="keep"
	) %>% 
	dplyr::ungroup() %>%
	dplyr::group_by(Year) %>%
	dplyr::summarize(
		SacPikeminnowIndex=mean(SacPikeminnow_CPUE_YM),
		.groups="keep"
	) %>% 
	dplyr::ungroup() %>%									 
	as.data.frame(.)
seineIndexDf


##########################################################################
## Figures:

sacpikeminnow_fig <- ggplot(seineIndexDf, aes(x=Year, y=SacPikeminnowIndex)) + 
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("Index") + 
  smr_x_axis(report_year, type = "all", season = "summer")+
  annotate("text", x=1968, y=0.6, label="Sites variable in\nearlier years", 
           hjust=0, size=2.7) + 
  stat_lt_avg(aes(y = SacPikeminnowIndex)) + 
	smr_caption(stat_name="the Pikeminnow index", report_year=report_year) + 
	smr_alttext(stat_name="Pikeminnow index")

sacpikeminnow_fig

getCaption(sacpikeminnow_fig)
getAlttext(sacpikeminnow_fig)


## Save plot:
DJFMP_sacpikeminnow_summer <- sacpikeminnow_fig

save(list="DJFMP_sacpikeminnow_summer", 
     file=file.path(fig_root_summer,"DJFMP_sacpikeminnow_summer.RData"))

