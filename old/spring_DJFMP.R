## Spring = March, April, May
## See a copy of the document "#22 Metadata (Updated May 30, 2019).doc" for reference.


##########################################################################
## Read in data:

thisDataRoot <- file.path(data_root,"DJFMP")

seineData <- read.csv(file.path(thisDataRoot,"seineData.csv"), stringsAsFactors=FALSE)
chippsData <- read.csv(file.path(thisDataRoot,"chippsData.csv"), 
                       stringsAsFactors=FALSE)
siteLatLong <- read.csv(file.path(thisDataRoot,"DJFMP_Site_Locations.csv",
												stringsAsFactors=FALSE)


##########################################################################
## Beach Seine: Splittail and Sacramento Pikeminnow

## Fill in missing volumes with an overall average:
unique(seineData$Volume)
seineData$Volume[is.na(seineData$Volume)] <- mean(seineData$Volume, na.rm=TRUE)

## Add fields:
seineData$Month <- lubridate::month(seineData$SampleDate)
seineData$Year <- lubridate::year(seineData$SampleDate)
seineData$Year_f <- as.factor(seineData$Year)



## Truncate the data according to the specified report year.
## Maybe keep 1995+ as a lazy way of dealing with sampling site inconsistencies.
seineDf <- subset(seineData, 1995 <= Year & Year <= report_year)

## Exclude DJFMP's region 1, which is north of Sacramento:
seineDf <- subset(seineDf, RegionCode != 1)

## Add lat/long:
seineDf <- dplyr::left_join(
	seineDf, 
	siteLatLong[ ,c("StationCode","latitude_location","longitude_location")],
	by="StationCode"
)

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



No DJFMP SPRING splittail
Yes DJFMP SUMMER beachseine sacramento pikeminnow (summer fish)


seineSpring <- subset(seineDf, Month %in% 3:5)
seineDf_summer <- subset(seineDf, Month %in% 6:8)






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
splittail_fig <- ggplot(seineIndexDf, aes(x=fyear, y=SplittailIndex)) +
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Splittail Index"))) + 
  lt_avg_line(lt_avg=mean(seineIndexDf$SplittailIndex, na.rm=TRUE)) + 
  std_x_axis_all_years(rpt_yr=report_year, "discrete")+
  std_x_axis_label("spring")+
  annotate("text", x = as.factor(1970), y = 0.2, 
           label = "Data collected intermittently before 1995", hjust = 0, size = 2)

ggsave(splittail_fig, file=file.path(fig_root_spring,"DJFMP_splittail.png"), 
       dpi=300, units="cm", width=9.3, height=6.8)


# sacpikeminnow_fig <- ggplot(seineIndexDf, aes(x=fyear, y=SacPikeminnowIndex)) +
  # geom_bar(stat="identity") +
  # theme_smr() +
  # theme(legend.position="none") + 
  # scale_y_continuous(expression(paste("Sacramento Pikeminnow Index"))) + 
  # lt_avg_line(lt_avg=mean(seineIndexDf$SacPikeminnowIndex, na.rm=TRUE)) + 
  # std_x_axis_all_years(rpt_yr=report_year, "discrete")+
  # std_x_axis_label("spring")+
  # annotate("text", x = as.factor(1970), y = 0.2, label = "Data not collected until 1995", hjust = 0, size = 2)
# sacpikeminnow_fig

# ggsave(sacpikeminnow_fig, file=file.path(fig_root_spring,"DJFMP_sacpikeminnow.png"), 
       # dpi=300, units="cm", width=9.3, height=6.8)


##########################################################################
## Chipps Trawl: Winterrun Chinook







			###################################

chippsDf <- subset(chippsDf_raw, Year <= report_year)

unique(allTrawlRaw$Location)
unique(allTrawlRaw$MethodCode)

unique(allTrawlRaw$MethodCode)
unique(allTrawlRaw$GearConditionCode)


chippsDf$Month <- lubridate::month(chippsDf$SampleDate)
chippsDf$Year <- lubridate::year(chippsDf$SampleDate)

unique(chippsDf$MethodCode)
unique(chippsDf$Location)
	
# # ## Maybe keep 1995+ for consistency with seine figure:
# # chippsDf <- subset(chippsDf, Year >= 1995)

## Fill in missing volumes with an overall average:
any(is.na(unique(chippsDf$Volume)))
chippsDf$Volume[is.na(chippsDf$Volume)] <- mean(chippsDf$Volume, na.rm=TRUE)

## Define spring data:
chippsDf_spring <- subset(chippsDf, Month %in% 3:5)
write.csv(chippsDf_spring, file.path(thisDataRoot,"springReportData_DJ_Chipps.csv"), 
					row.names=FALSE)

## Define summer data:
chippsDf_summer <- subset(chippsDf, Month %in% 6:8)
write.csv(chippsDf_summer, file.path(thisDataRoot,"summerReportData_DJ_Chipps.csv"), 
					row.names=FALSE)




			###################################
			
			
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
chippsIndexDf$fYear = factor(chippsIndexDf$Year)
chinook_winterByLength_fig <- ggplot(chippsIndexDf, 
																		 aes(x=fYear, y=chinook_winterByLengthIndex)) +
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Chinook Salmon Index\n(Winterrun, Unmarked Fish)"))) + 
  lt_avg_line(lt_avg=mean(chippsIndexDf$chinook_winterByLengthIndex, na.rm=TRUE)) + 
  std_x_axis_all_years(rpt_yr=report_year, "discrete")+
  std_x_axis_label("spring")

ggsave(chinook_winterByLength_fig, 
			 file=file.path(fig_root_spring,"DJFMP_chinook_winterByLength_allyears.png"), 
			 dpi=300, units="cm", width=9.3, height=6.8)

## Figures:
chinook_winterByLength_fig <- ggplot(chippsIndexDf, 
                                     aes(x=fYear, y=chinook_winterByLengthIndex)) +
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  std_x_axis_label("spring") + 	
  std_x_axis_rec_years(2018, "discrete") +
  scale_y_continuous(expression(paste("Chinook Salmon Index\n(Winterrun, Unmarked Fish)"))) + 
  geom_hline(yintercept=mean(chippsIndexDf$chinook_winterByLengthIndex, na.rm=TRUE), 
             col="red", linetype="dashed", size=0.9)

ggsave(chinook_winterByLength_fig, 
       file=file.path(fig_root_spring,"DJFMP_chinook_winterByLength_recyears.png"), 
       dpi=300, units="cm", width=9.3, height=6.8)

			 