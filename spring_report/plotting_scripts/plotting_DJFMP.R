## Spring = March, April, May
## See a copy of the document "#22 Metadata (Updated May 30, 2019).doc" for reference.

library(tidyverse)

projectRoot <- "."
reportRoot <- file.path(projectRoot,"spring_report")
dataRoot <- file.path(projectRoot,"data")
thisDataRoot <- file.path(dataRoot,"DJFMP")
figRoot <- file.path(reportRoot,"figures")

source(file.path(projectRoot, "IEP_Plot_Theme.R"))

##########################################################################
## Read in data:

seineDf <- read.csv(file.path(thisDataRoot,"springReportData_DJ_seine.csv"), 
										stringsAsFactors=FALSE)
chippsDf <- read.csv(file.path(thisDataRoot,"springReportData_DJ_Chipps.csv"), 
										 stringsAsFactors=FALSE)


##########################################################################
## Beach Seine: Splittail and Sacramento Pikeminnow

## These region designations came from Nick's code:														
seineDf$customRegion <- ""
seineDf$customRegion[seineDf$longitude_location < -122.216] <- "San Pablo Bay"
seineDf$customRegion[seineDf$longitude_location > -122.216 & 
										 seineDf$longitude_location < -121.829] <- "Suisun"
seineDf$customRegion[seineDf$longitude_location > -121.829] <- "The Delta"
unique(seineDf$customRegion)


## Create wide data frame:
seineDf$CommonName <- sub("Chinook salmon","Chinook salmon_",seineDf$CommonName)
seineDf$RaceByLength[is.na(seineDf$RaceByLength)] <- ""
seineDf$CommonName_RaceByLength <- with(seineDf, paste0(CommonName, RaceByLength))
keep_fields_seine <- c("Year","Month","Location","RegionCode","StationCode","SampleDate",
											 "SampleTime","MethodCode","GearConditionCode","TowNumber",
											 "Volume","CommonName_RaceByLength","Catch")
seineDfWide <- tidyr::spread(seineDf[ ,keep_fields_seine], CommonName_RaceByLength, 
														 Catch, fill=0)
seineDfWide$"No catch" <- NULL
names(seineDfWide) <- sub(" ", "_", names(seineDfWide))

head(seineDfWide)
lapply(split(seineDfWide$StationCode, seineDfWide$Year), unique)


## Calculate indices:
seine_CPUE_YM <- seineDfWide %>%
	dplyr::group_by(Year, Month) %>%
	dplyr::summarize(SplittailCPUE_YM = sum(splittail/Volume),
									 SacPikeminnowCPUE_YM = sum(Sacramento_pikeminnow/Volume)) %>% 
	as.data.frame(.)
seine_CPUE_YM

seineIndexDf <- seine_CPUE_YM %>%
	dplyr::group_by(Year) %>%
	dplyr::summarize(SplittailIndex = mean(SplittailCPUE_YM), 
									 SacPikeminnowIndex = mean(SacPikeminnowCPUE_YM)) %>% 
	as.data.frame(.)
seineIndexDf


## Figures:
splittail_fig <- ggplot(seineIndexDf, aes(x=Year, y=SplittailIndex)) +
  geom_bar(stat="identity") +
  theme_iep() +
  theme(legend.position="none") + 
  scale_x_continuous("Year (March - May)") + 
  scale_y_continuous(expression(paste("Splittail Index"))) + 
  geom_hline(yintercept=mean(seineIndexDf$SplittailIndex, na.rm=TRUE), col="red", 
             linetype="dashed", size=0.9)

ggsave(splittail_fig, file=file.path(figRoot,"DJFMP_splittail.png"), dpi=300, units="cm", 
			 width=9.3, height=6.8)


sacpikeminnow_fig <- ggplot(seineIndexDf, aes(x=Year, y=SacPikeminnowIndex)) +
  geom_bar(stat="identity") +
  theme_iep() +
  theme(legend.position="none") + 
  scale_x_continuous("Year (March - May)") + 
  scale_y_continuous(expression(paste("Sacramento Pikeminnow Index"))) + 
  geom_hline(yintercept=mean(seineIndexDf$SacPikeminnowIndex, na.rm=TRUE), col="red", 
             linetype="dashed", size=0.9)

ggsave(sacpikeminnow_fig, file=file.path(figRoot,"DJFMP_sacpikeminnow.png"), dpi=300, 
			 units="cm", width=9.3, height=6.8)



##########################################################################
## Chipps Trawl: Winterrun Chinook

## Create wide data frame:
chippsDf$CommonName <- sub("Chinook salmon","Chinook salmon_",chippsDf$CommonName)
chippsDf$CommonName <- sub(" ","_",chippsDf$CommonName)
chippsDf$RaceByLength[is.na(chippsDf$RaceByLength)] <- ""
chippsDf$CommonName_RaceByLength <- with(chippsDf, paste0(CommonName, RaceByLength))
keep_fields_chipps <- c("Year","Month","Location","RegionCode","StationCode",
												"SampleDate","SampleTime","MethodCode","GearConditionCode",
												"TowNumber","Volume","CommonName_RaceByLength","Catch")
chippsDfWide <- tidyr::spread(chippsDf[ ,keep_fields_chipps], CommonName_RaceByLength, 
															Catch, fill=0)
chippsDfWide$"No catch" <- NULL


## Calculate indices:
chipps_CPUE_YM <- chippsDfWide %>%
	dplyr::group_by(Year, Month) %>%
	dplyr::summarize(
		chinook_winterByLength_CPUE_YM = sum(Chinook_salmon_Winter/Volume)
	) %>% 
	as.data.frame(.)
chipps_CPUE_YM

chippsIndexDf <- chipps_CPUE_YM %>%
	dplyr::group_by(Year) %>%
	dplyr::summarize(
		chinook_winterByLengthIndex = mean(chinook_winterByLength_CPUE_YM) * 1000
	) %>% 
	as.data.frame(.)
chippsIndexDf


## Figures:
chinook_winterByLength_fig <- ggplot(chippsIndexDf, 
																		 aes(x=Year, y=chinook_winterByLengthIndex)) +
  geom_bar(stat="identity") +
  theme_iep() +
  theme(legend.position="none") + 
  scale_x_continuous("Year (March - May)") + 	
  scale_y_continuous(expression(paste("Chinook Salmon Index\n(Winterrun, Unmarked Fish)"))) + 
  geom_hline(yintercept=mean(chippsIndexDf$chinook_winterByLengthIndex, na.rm=TRUE), 
						 col="red", linetype="dashed", size=0.9)

ggsave(chinook_winterByLength_fig, 
			 file=file.path(figRoot,"DJFMP_chinook_winterByLength.png"), 
			 dpi=300, units="cm", width=9.3, height=6.8)


