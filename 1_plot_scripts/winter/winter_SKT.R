## Winter

##########################################################################
## Read in data:

thisDataRoot <- file.path(data_root,"SKT")

sktData <- read.csv(file.path(thisDataRoot,"sktData.csv"), stringsAsFactors=FALSE)

##########################################################################
## Spring Kodiak Trawl: Delta Smelt

## Add fields:
sktData$Month <- lubridate::month(sktData$SampleDate)
sktData$Year <- lubridate::year(sktData$SampleDate)
sktData$Year_f <- as.factor(sktData$Year)
sktData$SampleDate <- as.Date(as.character(sktData$SampleDate))
sktData$SampleTimeStart <- as.character(sktData$SampleTimeStart)
sktData$SampleTimeEnd <- as.character(sktData$SampleTimeEnd)
sktData$MeterCounts <- with(sktData, ifelse(MeterEnd < MeterStart, 
                                            ((1000000-MeterStart) + MeterEnd), 
                                            (MeterEnd - MeterStart)))
sktData$Volume_cubicm <- sktData$MeterCounts*0.02687*13.95


## Recreate SKT index (see "MEMO2015 SKT Delta Smelt Index.pdf" for instructions 
## on how to define the regions and calculate the index):
region_map <- list(
	"Confluence and West"=c("340","405","411","418","501","504","508","513","519","520",
													"602","606","609","610","801"),
	"Sacramento River System"=c("704","706","707","711","712","713","715","716","724"),
	"San Joaquin River System"=c("804","809","812","815","902","906","910","912","914",
															 "915","919","920","921","922","923")
)
region_map_inverse <- gsub("[0-9]+","",names(unlist(region_map)))
names(region_map_inverse) <- unlist(region_map)

## Keep the same surveys and use the same regions as CDFW:
catch_mat <- sktData %>%
  group_by(Year, Year_f, Month, SurveyNumber, SampleDate, StationCode,
           SampleTimeStart, SampleTimeEnd, Volume_cubicm) %>%
  summarize(DeltaSmelt=sum(Catch[CommonName == "delta smelt"]),
            .groups="keep") %>%
  ungroup()

catch_mat_sub <- catch_mat %>%
  filter(as.character(StationCode) %in% names(region_map_inverse) & 
					SurveyNumber %in% 1:4) %>%
  mutate(Region=region_map_inverse[as.character(StationCode)])

## Missing volume in 2021 at station 704. Do quick imputation:
tmp_that_stn <- subset(catch_mat_sub, Year == 2021 & StationCode == 704)
ind_missing_vol <- with(catch_mat_sub, which(Year == 2021 & StationCode == 704 & 
                                               is.na(Volume_cubicm)))
catch_mat_sub$Volume_cubicm[ind_missing_vol] <- mean(tmp_that_stn$Volume_cubicm, 
                                                     na.rm=TRUE)

## Calculate the index:
sktIndexDf <- catch_mat_sub %>%
  group_by(Year, Year_f, SurveyNumber, Region, StationCode) %>%
  summarize(Dens=sum(DeltaSmelt)/sum(Volume_cubicm), 
            .groups="keep") %>%
  ungroup() %>%
  group_by(Year, Year_f, SurveyNumber, Region) %>%
  summarize(Dens=mean(Dens), 
            .groups="keep") %>%
  ungroup() %>%
  group_by(Year, Year_f) %>%
  summarize(Index=10000*sum(Dens), 
            .groups="keep") %>%
  ungroup()

## SKT's methods were standardized by 2004. I think that's why they don't calculate 
## an index prior to that.
sktIndexDf$Index[sktIndexDf$Year <= 2003] <- NA
sktIndexDf <- subset(sktIndexDf, Year <= report_year)
sktIndexDf


##########################################################################
## Figures:

# SKT index uses its own set of months so don't use std_x_axis_label("winter")

SKT_delta_smelt_recent <- ggplot(sktIndexDf, aes(x=Year, y=Index)) + 
  geom_bar(stat="identity") +
  smr_theme_update() +
  ylab("Delta Smelt Index") + 
  stat_missing(size=2.5) + 
  stat_lt_avg() +
  stat_missing() + 
	smr_x_axis(report_year, type="recent", season="annual") + 
	smr_caption(stat_name="the Delta Smelt index", report_year=report_year) + 
	smr_alttext(stat_name="Delta Smelt index")

SKT_delta_smelt_recent

getCaption(SKT_delta_smelt_recent)
getAlttext(SKT_delta_smelt_recent)


save(list="SKT_delta_smelt_recent", 
		 file=file.path(fig_root_winter,"SKT_delta_smelt_recent.RData"))

