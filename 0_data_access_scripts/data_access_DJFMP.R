## Data retrieval script for DJFMP's Beach Seine and Trawl Surveys.
## Some of this code is taken directly from EDI.
## https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=244&revision=3
## 
## The main files are probably too large to store on GitHub, so filter now and 
## save smaller csv files.

##########################################################################

thisDataRoot <- file.path(data_root,"DJFMP")

## EDI URLs:
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/244/3/71c16ead9b8ffa4da7a52da180f601f4" 
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/244/3/93cb0e8ec9fa92adc7aba9499b3ea6d7" 
inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/244/3/64863836c6dc4f40f621f21cc03d1164" 
inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/244/3/17c9974d9b7b0125c146a887f3c64bd8" 
inUrl5  <- "https://pasta.lternet.edu/package/data/eml/edi/244/3/99a038d691f27cd306ff93fdcbc03b77" 

trawlFile1 <- file.path(thisDataRoot,
												"1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv")
trawlFile2 <- file.path(thisDataRoot,
												"2002-2018_DJFMP_trawl_fish_and_water_quality_data.csv")
seineFile <- file.path(thisDataRoot,
											 "1976-2018_DJFMP_beach_seine_fish_and_water_quality_data.csv")
taxonomyFile <- file.path(thisDataRoot,"DJFMP_Fish_Taxonomy.csv")
siteFile <- file.path(thisDataRoot,"DJFMP_Site_Locations.csv")


##########################################################################
## Retrieve data from EDI:

download.file(url=inUrl1, destfile=trawlFile1)
download.file(url=inUrl2, destfile=trawlFile2)
download.file(url=inUrl3, destfile=seineFile)
download.file(url=inUrl4, destfile=taxonomyFile)
download.file(url=inUrl5, destfile=siteFile)

seineDfRaw <- read.csv(seineFile, stringsAsFactors=FALSE)
trawlDfRaw_1 <- read.csv(trawlFile1, stringsAsFactors=FALSE)
trawlDfRaw_2 <- read.csv(trawlFile2, stringsAsFactors=FALSE)
siteLatLong <- read.csv(siteFile, stringsAsFactors=FALSE) 


##########################################################################
## Create reduced data files:

## Each record in the original data represents a unique 
## sample-species-length-race-etc. Group the data to reduce size.

## Beach Seine:
seineData <- seineDfRaw %>%
	dplyr::group_by(Location, RegionCode, StationCode, SampleDate, SampleTime,
									MethodCode, GearConditionCode, TowNumber, Volume, CommonName, 
									RaceByLength) %>% 
	dplyr::summarize(Catch = sum(Count),
	                 .groups="keep") %>%
	dplyr::ungroup() %>% 
	as.data.frame(.)

unique(seineData$MethodCode)
unique(seineData$GearConditionCode)

	
## Chipps Trawl:
allTrawlRaw <- rbind(trawlDfRaw_1, trawlDfRaw_2)

unique(allTrawlRaw$Location)
unique(allTrawlRaw$MethodCode)
unique(allTrawlRaw$GearConditionCode)


chippsData <- allTrawlRaw %>%
	dplyr::filter(Location == "Chipps Island" & MethodCode == "MWTR" & 
								GearConditionCode %in% 1:3) %>%
	dplyr::group_by(Location, RegionCode, StationCode, SampleDate, SampleTime,
									MethodCode, GearConditionCode, TowNumber, Volume, CommonName, 
									RaceByLength) %>% 
	dplyr::summarize(Catch = sum(Count),
	                 .groups="keep") %>%
	dplyr::ungroup() %>% 
	as.data.frame(.)

unique(chippsData$MethodCode)
unique(chippsData$Location)


##########################################################################
## Save reduced data files:

write.csv(seineData, file.path(thisDataRoot,"seineData.csv"), row.names=FALSE)
write.csv(chippsData, file.path(thisDataRoot,"chippsData.csv"), row.names=FALSE)


##########################################################################
## Remove large files:

unlink(trawlFile1)
unlink(trawlFile2)
unlink(seineFile)
unlink(taxonomyFile)
# unlink(siteFile)

