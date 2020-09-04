## Data retrieval script for CDFW's 20mm Survey.
## Database is too large to store on GitHub, so query now and save smaller csv files.
# As of 7/15/2020 data are now posted as .csvs on EDI, so it will be easier to query.

library(smonitr)

projectRoot <- "."
dataRoot <- file.path(projectRoot,"data")
thisDataRoot <- file.path(dataRoot,"20mm")

#DOWNLOD catch data from EDI
twentymill = get_edi_data(535, c("qry_EDI_20mm-catch_06022020.csv"), guess_max = 1000000)
twentymill = twentymill[[1]]

#import file of stations used for the index
index_stations <- read.csv(file.path(thisDataRoot, "20mm-index-stations.csv"),
                           stringsAsFactors=FALSE)

#get rid of the data that isn't used for the index
twentymillindex = merge(twentymill, index_stations)

#replace missing volumes with average volume
twentymillindex$Volume[which(is.na(twentymillindex$Volume))]  = mean(twentymillindex$Volume, na.rm = T)

#subset Delta Smelt and calculate CPUE
dsmIndexDf2 = select(twentymillindex, c("Year":"Volume", "Delta_Smelt", "Station")) %>%
  mutate(CPUE = Delta_Smelt/Volume*10000)


#Combine station CPUE and log-transfor
dsmIndexDf = group_by(dsmIndexDf2, Year, Survey, Station) %>%
  summarize(logCPUE = log((mean(CPUE)+1), base =10))

#The index is calculated based on different surveys for each year, 
#depending on when the lengths reach a certain piont
dsmIndexDf$UseforIndex <- FALSE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 1995 & dsmIndexDf$Survey %in% 2:5] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 1996 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 1997 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 1998 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 1999 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2000 & dsmIndexDf$Survey %in% 5:8] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2001 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2002 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2003 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2004 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2005 & dsmIndexDf$Survey %in% 5:8] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2006 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2007 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2008 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2009 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2010 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2011 & dsmIndexDf$Survey %in% 5:8] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2012 & dsmIndexDf$Survey %in% 5:8] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2013 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2014 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2015 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2016 & dsmIndexDf$Survey %in% 2:5] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2017 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2018 & dsmIndexDf$Survey %in% c(1,2,3,9)] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2019 & dsmIndexDf$Survey %in% 3:6] <- TRUE


#Calculate the mean of the log-transformed data per survey and calculate the 
# geometric mean of the survey by back-transformation and sbutracting 1

dsmIndex = filter(dsmIndexDf, UseforIndex == TRUE) %>%
  group_by(Year, Survey) %>%
  summarize(meanCPUE = mean(logCPUE), geomean = (10^(meanCPUE))-1)


dsmIndexYear = group_by(dsmIndex, Year) %>%
  summarize(index = sum(geomean))











###################################################################################################
#this is the old code that deals with the database.


## 20mm Survey url:
surveyURL <- "ftp://ftp.dfg.ca.gov/Delta%20Smelt/20mm_New.zip"

## Name of 20mm Survey zip file: 
zipFileName <- "20mm_New.zip"

## Name of 20mm Survey database within the zip file:
dbName <- "20mm_New.accdb"

##########################################################################
## Retrieve 20mm Survey database copy:

## Download and unzip the file:
localZipFile <- file.path(thisDataRoot, zipFileName)

download.file(url=surveyURL, destfile=localZipFile)
localDbFile <- unzip(zipfile=localZipFile, exdir=thisDataRoot)


##########################################################################
## Connect to database:

## Open connection to the database:
dbString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
									 "Dbq=",localDbFile)
con <- DBI::dbConnect(drv=odbc::odbc(), .connection_string=dbString)

tables <- odbc::dbListTables(conn=con)
tables

# towTable <- DBI::dbReadTable(con, "Tow")
# catchTable <- DBI::dbReadTable(con, "FishSample")
# lengthTable <- DBI::dbReadTable(con, "FishLength")
# surveyTable <- DBI::dbReadTable(con, "Survey")
# fishCodeTable <- DBI::dbReadTable(con, "FishCodes")
# stationTable <- DBI::dbReadTable(con, "Station")

index_stations <- read.csv(file.path(thisDataRoot, "20mm-index-stations.csv"),
													 stringsAsFactors=FALSE)

##########################################################################
## Delta Smelt

qry_AvgDSLength <- DBI::dbGetQuery(con, "
SELECT Year([SampleDate]) AS [Year], Survey.Survey, FishSample.FishCode, 
Avg(FishLength.Length) AS AvgOfLength
FROM (((Survey INNER JOIN Station ON Survey.SurveyID=Station.SurveyID) INNER JOIN Tow ON
Station.StationID=Tow.StationID) INNER JOIN Gear ON Tow.TowID=Gear.TowID) INNER JOIN ((FishSample
INNER JOIN FishLength ON FishSample.FishSampleID=FishLength.FishSampleID) INNER JOIN LabSample ON
FishSample.LabSampleID=LabSample.LabSampleID) ON Gear.GearID=LabSample.GearID
WHERE (((FishLength.Length)<60))
GROUP BY Year([SampleDate]), Survey.Survey, FishSample.FishCode
HAVING (((FishSample.FishCode)=3))
ORDER BY Year([SampleDate]) DESC , Survey.Survey;
")


qry_MaxofTow <- DBI::dbGetQuery(con, "
SELECT Year([SampleDate]) AS [Year], Survey.Survey, Survey.SampleDate, Station.Station,
Count(Tow.TowNum) AS CountOfTowNum
FROM Survey INNER JOIN (Tow INNER JOIN Station ON Tow.StationID=Station.StationID) ON
Survey.SurveyID=Station.SurveyID
GROUP BY Year([SampleDate]), Survey.Survey, Survey.SampleDate, Station.Station;
")


qry_TLT_DS_CPUE_01 <- DBI::dbGetQuery(con, "
SELECT Survey.SampleDate, Survey.Survey, Station.Station, Tow.TowNum, Gear.GearCode, 
Gear.MeterSerial, Gear.MeterCheck AS D, 1.51*0.02687*[D] AS Vt
FROM ((Survey INNER JOIN Station ON Survey.SurveyID=Station.SurveyID) INNER JOIN Tow ON
Station.StationID=Tow.StationID) INNER JOIN Gear ON Tow.TowID=Gear.TowID
WHERE (((Gear.GearCode)=2))
ORDER BY Survey.SampleDate, Survey.Survey, Station.Station, Tow.TowNum;
")


qry_TLT_DS_CPUE_02 <- DBI::dbGetQuery(con, "
SELECT Survey.SampleDate, Station.Station, Tow.TowNum, Count(FishLength.Length) AS CountOfLength,
FishCodes.[Common Name], FishSample.Catch, FishSample.FishCode
FROM ((((((Survey INNER JOIN Station ON Survey.SurveyID=Station.SurveyID) INNER JOIN Tow ON
Station.StationID=Tow.StationID) INNER JOIN Gear ON Tow.TowID=Gear.TowID) INNER JOIN LabSample
ON Gear.GearID=LabSample.GearID) INNER JOIN FishSample ON
LabSample.LabSampleID=FishSample.LabSampleID) INNER JOIN FishCodes ON
FishSample.FishCode=FishCodes.[Fish Code]) INNER JOIN FishLength ON
FishSample.FishSampleID=FishLength.FishSampleID
WHERE (((FishLength.Length)<60))
GROUP BY Survey.SampleDate, Station.Station, Tow.TowNum, FishCodes.[Common Name], 
FishSample.Catch, FishSample.FishCode
HAVING (((FishSample.FishCode)=3));
")


# qry_TLT_DS_CPUE_03 <- DBI::dbGetQuery(con, '
# SELECT [qry_TLT_DS CPUE 01].SampleDate, [qry_TLT_DS CPUE 01].Survey, [qry_TLT_DS CPUE 01].Station,
# [qry_TLT_DS CPUE 01].TowNum, IIf([Common Name] Is Null,"No DS Catch",[Common Name]) AS Species,
# IIf([CountOfLength] Is Null,0,[CountofLength]) AS [DS Catch], [qry_TLT_DS CPUE 01].Vt AS [Volume of Water]
# FROM [qry_TLT_DS CPUE 01] LEFT JOIN [qry_TLT_DS CPUE 02] ON ([qry_TLT_DS CPUE
# 01].SampleDate=[qry_TLT_DS CPUE 02].SampleDate) AND ([qry_TLT_DS CPUE 01].Station=[qry_TLT_DS CPUE 02].Station) AND ([qry_TLT_DS CPUE 01].TowNum=[qry_TLT_DS CPUE 02].TowNum)
# GROUP BY [qry_TLT_DS CPUE 01].SampleDate, [qry_TLT_DS CPUE 01].Survey, [qry_TLT_DS CPUE 01].Station, [qry_TLT_DS CPUE 01].TowNum, IIf([Common Name] Is Null,"No DS Catch",[Common Name]),
# IIf([CountOfLength] Is Null,0,[CountofLength]), [qry_TLT_DS CPUE 01].Vt;
# ')
qry_TLT_DS_CPUE_03 <- dplyr::left_join(qry_TLT_DS_CPUE_01, qry_TLT_DS_CPUE_02,
																			 by=c("SampleDate","Station","TowNum"))

qry_TLT_DS_CPUE_03$VolumeOfWater <- qry_TLT_DS_CPUE_03$Vt
qry_TLT_DS_CPUE_03$VolumeOfWater[is.na(qry_TLT_DS_CPUE_03$VolumeOfWater)] <- 
	mean(qry_TLT_DS_CPUE_03$VolumeOfWater, na.rm=TRUE)

qry_TLT_DS_CPUE_03$Species <- qry_TLT_DS_CPUE_03$"Common Name"
qry_TLT_DS_CPUE_03$Species[is.na(qry_TLT_DS_CPUE_03$Species)] <- "No DS Catch"

qry_TLT_DS_CPUE_03$DS_Catch <- qry_TLT_DS_CPUE_03$CountOfLength
qry_TLT_DS_CPUE_03$DS_Catch[is.na(qry_TLT_DS_CPUE_03$DS_Catch)] <- 0

qry_TLT_DS_CPUE_03$Year <- lubridate::year(qry_TLT_DS_CPUE_03$SampleDate)



# qry_TLT_DS_CPUE_04 <- DBI::dbGetQuery(con, "
# SELECT Year([SampleDate]) AS [Year], [qry_TLT_DS CPUE 03].[qry_TLT_DS CPUE 01].Survey, [qry_TLT_DS
# CPUE 03].SampleDate, [qry_TLT_DS CPUE 03].[qry_TLT_DS CPUE 01].Station, Sum([qry_TLT_DS CPUE
# 03].[DS Catch]) AS [SumOfDS Catch], Sum([DS Catch]/[Volume of Water]*10000) AS Nt
# FROM [qry_TLT_DS CPUE 03]
# GROUP BY Year([SampleDate]), [qry_TLT_DS CPUE 03].[qry_TLT_DS CPUE 01].Survey, [qry_TLT_DS CPUE
# 03].SampleDate, [qry_TLT_DS CPUE 03].[qry_TLT_DS CPUE 01].Station;
# ")
qry_TLT_DS_CPUE_04 <- qry_TLT_DS_CPUE_03 %>% 
	dplyr::group_by(Year, Survey, SampleDate, Station) %>%
	dplyr::summarize(SumOfDS_Catch = sum(DS_Catch),
									 Nt = sum(DS_Catch/VolumeOfWater * 10000))



# qry_TLT_DS CPUE <- DBI::dbGetQuery(con, "
# SELECT [qry_TLT_DS CPUE 04].Year, [qry_TLT_DS CPUE 04].[qry_TLT_DS CPUE 01].Survey, [qry_TLT_DS
# CPUE 04].SampleDate, [qry_TLT_DS CPUE 04].[qry_TLT_DS CPUE 01].Station, [Nt]/[CountOfTowNum] AS
# CPUE
# FROM [qry_TLT_DS CPUE 04] INNER JOIN qry_MaxofTow ON ([qry_TLT_DS CPUE 04].SampleDate =
# qry_MaxofTow.SampleDate) AND ([qry_TLT_DS CPUE 04].[qry_TLT_DS CPUE 01].Station =
# qry_MaxofTow.Station)
# GROUP BY [qry_TLT_DS CPUE 04].Year, [qry_TLT_DS CPUE 04].[qry_TLT_DS CPUE 01].Survey, [qry_TLT_DS
# CPUE 04].SampleDate, [qry_TLT_DS CPUE 04].[qry_TLT_DS CPUE 01].Station, [Nt]/[CountOfTowNum];
# ")
## Unclear about this query .... 
qry_TLT_DS_CPUE <- dplyr::inner_join(qry_TLT_DS_CPUE_04, qry_MaxofTow, 
																		 by=c("Year","Survey","SampleDate","Station"))
qry_TLT_DS_CPUE <- qry_TLT_DS_CPUE %>%
	dplyr::mutate(CPUE = Nt/CountOfTowNum) %>%
	dplyr::group_by(Year, Survey, SampleDate, Station, CPUE) %>%
	dplyr::ungroup()		#???



# qry_TLT_Index01 <- DBI::dbGetQuery(con, "
# SELECT [qry_TLT_DS CPUE].Year, [qry_TLT_DS CPUE].[qry_TLT_DS CPUE 01].Survey, [qry_TLT_DS
# CPUE].SampleDate, tbl_TLT_IndexStations.Station, [qry_TLT_DS CPUE].CPUE, [CPUE]+1 AS [CPUE+1],
# Log([CPUE]+1)/Log(10) AS [Log10 Trans]
# FROM [qry_TLT_DS CPUE] INNER JOIN tbl_TLT_IndexStations ON [qry_TLT_DS CPUE].[qry_TLT_DS CPUE
# 01].Station = tbl_TLT_IndexStations.Station;
# ")
qry_TLT_Index01 <- dplyr::inner_join(qry_TLT_DS_CPUE, index_stations, by="Station")
qry_TLT_Index01 <- qry_TLT_Index01 %>%
	dplyr::mutate(CPUE_plus_1 = CPUE + 1,
								Log10Trans = log10(CPUE_plus_1)) %>%
	dplyr::select(Year, Survey, SampleDate, Station, CPUE, CPUE_plus_1, Log10Trans)



# qry_TLT_Index02 <- DBI::dbGetQuery(con, "
# SELECT qry_TLT_Index01.Year, qry_TLT_Index01.[qry_TLT_DS CPUE 01].Survey,
# Avg(qry_TLT_Index01.[Log10 Trans]) AS [AvgOfLog10 Trans], (10^(Avg([Log10 Trans])))-1 AS Geomean,
# qry_AvgDSLength.AvgOfLength
# FROM qry_TLT_Index01 INNER JOIN qry_AvgDSLength ON (qry_TLT_Index01.Year =
# qry_AvgDSLength.Year) AND (qry_TLT_Index01.[qry_TLT_DS CPUE 01].Survey = qry_AvgDSLength.Survey)
# GROUP BY qry_TLT_Index01.Year, qry_TLT_Index01.[qry_TLT_DS CPUE 01].Survey,
# qry_AvgDSLength.AvgOfLength;
# ")
qry_TLT_Index02 <- dplyr::inner_join(qry_TLT_Index01, qry_AvgDSLength, 
																		 by=c("Year","Survey"))
qry_TLT_Index02 <- qry_TLT_Index02 %>%
	dplyr::group_by(Year, Survey, AvgOfLength) %>%
	dplyr::summarize(AvgOfLog10Trans = mean(Log10Trans),
									 Geomean = (10^(AvgOfLog10Trans))-1) %>% 
	as.data.frame(.)


dsmIndexDf <- qry_TLT_Index02
split(dsmIndexDf, dsmIndexDf$Year)

dsmIndexDf$UseforIndex <- FALSE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 1995 & dsmIndexDf$Survey %in% 2:5] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 1996 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 1997 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 1998 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 1999 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2000 & dsmIndexDf$Survey %in% 5:8] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2001 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2002 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2003 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2004 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2005 & dsmIndexDf$Survey %in% 5:8] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2006 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2007 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2008 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2009 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2010 & dsmIndexDf$Survey %in% 4:7] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2011 & dsmIndexDf$Survey %in% 5:8] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2012 & dsmIndexDf$Survey %in% 5:8] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2013 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2014 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2015 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2016 & dsmIndexDf$Survey %in% 2:5] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2017 & dsmIndexDf$Survey %in% 3:6] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2018 & dsmIndexDf$Survey %in% c(1,2,3,9)] <- TRUE
dsmIndexDf$UseforIndex[dsmIndexDf$Year == 2019 & dsmIndexDf$Survey %in% 3:6] <- TRUE

dsmIndexDf <- dsmIndexDf %>%
	dplyr::filter(UseforIndex) %>%
	dplyr::group_by(Year) %>%
	dplyr::summarize(Index=sum(Geomean)) %>% 
	dplyr::ungroup() %>% 
	dplyr::mutate(Index = round(Index,1)) %>%
	as.data.frame(.)

dsmIndexDf$Index[dsmIndexDf$Year == 2018] <- NA
dsmIndexDf


##########################################################################
## Longfin Smelt

qry_AvgLSLength <- DBI::dbGetQuery(con, "
SELECT Year([SampleDate]) AS [Year], Survey.Survey, FishSample.FishCode, 
Avg(FishLength.Length) AS AvgOfLength
FROM (((Survey INNER JOIN Station ON Survey.SurveyID=Station.SurveyID) INNER JOIN Tow ON
Station.StationID=Tow.StationID) INNER JOIN Gear ON Tow.TowID=Gear.TowID) INNER JOIN ((FishSample
INNER JOIN FishLength ON FishSample.FishSampleID=FishLength.FishSampleID) INNER JOIN LabSample ON
FishSample.LabSampleID=LabSample.LabSampleID) ON Gear.GearID=LabSample.GearID
WHERE (((FishLength.Length)<60))
GROUP BY Year([SampleDate]), Survey.Survey, FishSample.FishCode
HAVING (((FishSample.FishCode)=2))
ORDER BY Year([SampleDate]) DESC , Survey.Survey;
")


qry_MaxofTow <- DBI::dbGetQuery(con, "
SELECT Year([SampleDate]) AS [Year], Survey.Survey, Survey.SampleDate, Station.Station,
Count(Tow.TowNum) AS CountOfTowNum
FROM Survey INNER JOIN (Tow INNER JOIN Station ON Tow.StationID=Station.StationID) ON
Survey.SurveyID=Station.SurveyID
GROUP BY Year([SampleDate]), Survey.Survey, Survey.SampleDate, Station.Station;
")


qry_TLT_LS_CPUE_01 <- DBI::dbGetQuery(con, "
SELECT Survey.SampleDate, Survey.Survey, Station.Station, Tow.TowNum, Gear.GearCode, 
Gear.MeterSerial, Gear.MeterCheck AS D, 1.51*0.02687*[D] AS Vt
FROM ((Survey INNER JOIN Station ON Survey.SurveyID=Station.SurveyID) INNER JOIN Tow ON
Station.StationID=Tow.StationID) INNER JOIN Gear ON Tow.TowID=Gear.TowID
WHERE (((Gear.GearCode)=2))
ORDER BY Survey.SampleDate, Survey.Survey, Station.Station, Tow.TowNum;
")


qry_TLT_LS_CPUE_02 <- DBI::dbGetQuery(con, "
SELECT Survey.SampleDate, Station.Station, Tow.TowNum, Count(FishLength.Length) AS CountOfLength,
FishCodes.[Common Name], FishSample.Catch, FishSample.FishCode
FROM ((((((Survey INNER JOIN Station ON Survey.SurveyID=Station.SurveyID) INNER JOIN Tow ON
Station.StationID=Tow.StationID) INNER JOIN Gear ON Tow.TowID=Gear.TowID) INNER JOIN LabSample
ON Gear.GearID=LabSample.GearID) INNER JOIN FishSample ON
LabSample.LabSampleID=FishSample.LabSampleID) INNER JOIN FishCodes ON
FishSample.FishCode=FishCodes.[Fish Code]) INNER JOIN FishLength ON
FishSample.FishSampleID=FishLength.FishSampleID
WHERE (((FishLength.Length)<60))
GROUP BY Survey.SampleDate, Station.Station, Tow.TowNum, FishCodes.[Common Name], 
FishSample.Catch, FishSample.FishCode
HAVING (((FishSample.FishCode)=2));
")


qry_TLT_LS_CPUE_03 <- dplyr::left_join(qry_TLT_LS_CPUE_01, qry_TLT_LS_CPUE_02,
																			 by=c("SampleDate","Station","TowNum"))

qry_TLT_LS_CPUE_03$VolumeOfWater <- qry_TLT_LS_CPUE_03$Vt
qry_TLT_LS_CPUE_03$VolumeOfWater[is.na(qry_TLT_LS_CPUE_03$VolumeOfWater)] <- 
	mean(qry_TLT_LS_CPUE_03$VolumeOfWater, na.rm=TRUE)

qry_TLT_LS_CPUE_03$Species <- qry_TLT_LS_CPUE_03$"Common Name"
qry_TLT_LS_CPUE_03$Species[is.na(qry_TLT_LS_CPUE_03$Species)] <- "No LS Catch"

qry_TLT_LS_CPUE_03$LS_Catch <- qry_TLT_LS_CPUE_03$CountOfLength
qry_TLT_LS_CPUE_03$LS_Catch[is.na(qry_TLT_LS_CPUE_03$LS_Catch)] <- 0

qry_TLT_LS_CPUE_03$Year <- lubridate::year(qry_TLT_LS_CPUE_03$SampleDate)


qry_TLT_LS_CPUE_04 <- qry_TLT_LS_CPUE_03 %>% 
	dplyr::group_by(Year, Survey, SampleDate, Station) %>%
	dplyr::summarize(SumOfLS_Catch = sum(LS_Catch),
									 Nt = sum(LS_Catch/VolumeOfWater * 10000))


qry_TLT_LS_CPUE <- dplyr::inner_join(qry_TLT_LS_CPUE_04, qry_MaxofTow, 
																		 by=c("Year","Survey","SampleDate","Station"))
qry_TLT_LS_CPUE <- qry_TLT_LS_CPUE %>%
	dplyr::mutate(CPUE = Nt/CountOfTowNum) %>%
	dplyr::group_by(Year, Survey, SampleDate, Station, CPUE) %>%
	dplyr::ungroup()


qry_TLT_Index01_LFS <- dplyr::inner_join(qry_TLT_LS_CPUE, index_stations, by="Station")
qry_TLT_Index01_LFS <- qry_TLT_Index01_LFS %>%
	dplyr::mutate(CPUE_plus_1 = CPUE + 1,
								Log10Trans = log10(CPUE_plus_1)) %>%
	dplyr::select(Year, Survey, SampleDate, Station, CPUE, CPUE_plus_1, Log10Trans)


qry_TLT_Index02_LFS <- dplyr::inner_join(qry_TLT_Index01_LFS, qry_AvgLSLength, 
																		 by=c("Year","Survey"))
qry_TLT_Index02_LFS <- qry_TLT_Index02_LFS %>%
	dplyr::group_by(Year, Survey, AvgOfLength) %>%
	dplyr::summarize(AvgOfLog10Trans = mean(Log10Trans),
									 Geomean = (10^(AvgOfLog10Trans))-1) %>% 
	as.data.frame(.)


lfsIndexDf <- qry_TLT_Index02_LFS
split(lfsIndexDf, lfsIndexDf$Year)

lfsIndexDf <- lfsIndexDf %>%
	dplyr::filter(Survey %in% 1:4) %>%
	dplyr::group_by(Year) %>%
	dplyr::summarize(Index=sum(Geomean)) %>% 
	dplyr::ungroup() %>% 
	dplyr::mutate(Index = round(Index,1)) %>%
	as.data.frame(.)

lfsIndexDf

	
##########################################################################
## Disconnect from database, delete database, and save csv files:

DBI::dbDisconnect(conn=con)

unlink(localZipFile)
unlink(localDbFile)

write.csv(dsmIndexDf, file.path(thisDataRoot,"20mm_DSM_index.csv"), row.names=FALSE)
write.csv(lfsIndexDf, file.path(thisDataRoot,"20mm_LFS_index.csv"), row.names=FALSE)

