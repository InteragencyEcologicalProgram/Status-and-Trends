## Data retrieval script for DJFMP's Beach Seine and Trawl Surveys.
## Some of this code is taken directly from EDI.
## https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=244&revision=7
## 
## The main files are probably too large to store on GitHub, so filter now and 
## save smaller csv files.

##########################################################################


DJFMP = get_edi_data(244, c("1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv", 
                            "2002-2020_DJFMP_trawl_fish_and_water_quality_data.csv",
                            "1976-2020_DJFMP_beach_seine_fish_and_water_quality_data.csv",
                            "DJFMP_Fish_Taxonomy.csv",
                            "DJFMP_Site_Locations.csv"), guess_max = 1000000  )

trawlDfRaw_1<- DJFMP[["1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv"]]
trawlDfRaw_2<- DJFMP[["2002-2020_DJFMP_trawl_fish_and_water_quality_data.csv"]]
seineDfRaw <- DJFMP[["1976-2020_DJFMP_beach_seine_fish_and_water_quality_data.csv"]]
taxonomyFile <- DJFMP[["DJFMP_Fish_Taxonomy.csv"]]
siteLatLong <- DJFMP[["DJFMP_Site_Locations.csv"]]


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
save(seineData, siteLatLong, file = file.path(data_root,"seineData.RData"))
save(chippsData, file = file.path(data_root,"chippsData.RData"))

#write.csv(seineData, file.path(thisDataRoot,"seineData.csv"), row.names=FALSE)
#write.csv(chippsData, file.path(thisDataRoot,"chippsData.csv"), row.names=FALSE)


##########################################################################
## Remove large files:

#unlink(trawlFile1)
#unlink(trawlFile2)
#unlink(seineFile)
#unlink(taxonomyFile)
# unlink(siteFile)

