## Summer = June, July, August

library(tidyverse)

##########################################################################
## Read in data:

seineDf_raw <- read.csv(file.path(data_root,"DJFMP","summerReportData_DJ_seine.csv"), 
                        stringsAsFactors=FALSE)

## Truncate the data according to the specified report year:
seineDf <- subset(seineDf_raw, Year <= report_year)

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
									 SacPikeminnowCPUE_YM = sum(Sacramento_pikeminnow/Volume),
									 .groups="keep") %>% 
	as.data.frame(.)
seine_CPUE_YM

seineIndexDf <- seine_CPUE_YM %>%
	dplyr::group_by(Year) %>%
	dplyr::summarize(SplittailIndex = mean(SplittailCPUE_YM), 
									 SacPikeminnowIndex = mean(SacPikeminnowCPUE_YM),
									 .groups="keep") %>% 
	as.data.frame(.)
seineIndexDf


## Figures:

#turn year into a factor so the standardized scale works
seineIndexDf = mutate(seineIndexDf, fyear = as.factor(Year))

all_yrs <- min(seineIndexDf$Year):max(seineIndexDf$Year)
missing_yrs <- all_yrs[!(all_yrs %in% seineIndexDf$Year)]
missing_yrs_df <- data.frame("Year"=missing_yrs, "Result"=0)

sacpikeminnow_fig <- ggplot(seineIndexDf, aes(x=fyear, y=SacPikeminnowIndex)) +
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Sacramento Pikeminnow Index"))) + 
  lt_avg_line(lt_avg=mean(seineIndexDf$SacPikeminnowIndex, na.rm=TRUE)) + 
  std_x_axis_all_years(rpt_yr=report_year, "discrete")+
  std_x_axis_label("summer") + 
  missing_data_symb(df=seineIndexDf, yr_var=fyear, rpt_yr=report_year, symb_size=1) + 
  annotate("text", x = as.factor(1968), y=0.5, label = "Data not\ncollected\nuntil 1976", 
           hjust = 0, size = 2)
sacpikeminnow_fig

ggsave(sacpikeminnow_fig, 
       file=file.path(fig_root_summer,"DJFMP_sacpikeminnow_summer.png"), 
       dpi=300, units="cm", width=9.3, height=6.8)

