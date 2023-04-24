## Winter = Dec, Jan, Feb
## See a copy of the document "#22 Metadata (Updated May 30, 2019).doc" for reference.

source("setup.R")

library(lubridate)

##########################################################################
## Read in data:

load(file.path(data_root, "chippsData.RData"))

##########################################################################
## Chipps Trawl: Winterrun Chinook

## Fill in missing volumes with an overall average:
any(is.na(unique(chippsData$Volume)))
chippsData$Volume[is.na(chippsData$Volume)] <- mean(chippsData$Volume, na.rm=TRUE)

## Add fields:
chippsData = mutate(chippsData, 
                    Month = month(SampleDate),
                    Year = year(SampleDate),
                    Year_f = as.factor(Year))
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
                              Month %in% c(12,1,2))

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
    chinook_winterByLengthIndex=mean(chinook_winterByLength_CPUE_YM, na.rm = TRUE) * 1000,
    .groups="keep"
  ) %>% 
  dplyr::ungroup() %>%
  as.data.frame(.)
chippsIndexDf


##########################################################################
## Figures:

# use_ylab <- expression(paste("Chinook Salmon Index\n(Winterrun, Unmarked Fish)"))
use_ylab <- "Index for unmarked fish"

## All years:
Chipps_all_years_WN <- ggplot(chippsIndexDf, aes(x=Year, y=chinook_winterByLengthIndex)) + 
  geom_bar(stat="identity") +
  smr_theme_update() + 
  smr_x_axis(report_year, "all", "winter") + 
  ylab(use_ylab) +
  stat_lt_avg() + 
  annotate("text", x=1968, y=0.15, label="Earlier data\nomitted", 
           hjust=0, size=2.7) + 
	smr_caption(stat_name="the juvenile winter-run Chinook Salmon passage rate", 
							report_year=report_year) + 
	smr_alttext(stat_name="juvenile winter-run Chinook Salmon passage rate")

Chipps_all_years_WN

getCaption(Chipps_all_years_WN)
getAlttext(Chipps_all_years_WN)


## Recent years:
Chipps_all_recent_WN <- 
	ggplot(chippsIndexDf, aes(x=Year, y=chinook_winterByLengthIndex)) +
  geom_bar(stat="identity") + 
  smr_theme_update() + 
  smr_x_axis(report_year, "recent", "winter")+ 
  ylab(use_ylab)+
  stat_lt_avg() + 
	smr_caption(stat_name="the juvenile winter-run Chinook Salmon passage rate", 
							report_year=report_year) + 
	smr_alttext(stat_name="juvenile winter-run Chinook Salmon passage rate")

Chipps_all_recent_WN

getCaption(Chipps_all_recent_WN)
getAlttext(Chipps_all_recent_WN)


## Save plots:
DJFMP_chinook_winterByLength <- list()
DJFMP_chinook_winterByLength[["Chipps_all_years_WN"]] <- Chipps_all_years_WN
DJFMP_chinook_winterByLength[["Chipps_all_recent_WN"]] <- Chipps_all_recent_WN

save(list="DJFMP_chinook_winterByLength", 
		 file=file.path(fig_root_winter,"DJFMP_chinook_winterByLength.RData"))

