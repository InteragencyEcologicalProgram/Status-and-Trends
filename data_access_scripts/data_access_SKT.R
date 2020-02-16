## Data retrieval script for CDFW's SKT Survey.

projectRoot <- "."
dataRoot <- file.path(projectRoot,"data")
thisDataRoot <- file.path(dataRoot,"SKT")

## SKT Survey url:
surveyURL <- "ftp://ftp.wildlife.ca.gov/Delta%20Smelt/SKT.zip"

## Name of SKT Survey zip file: 
zipFileName <- "SKT.zip"

## Name of SKT Survey database within the zip file:
dbName <- "SKT.accdb"

##########################################################################
## Retrieve SKT Survey database copy:

## Download and unzip the file:
localZipFile <- file.path(thisDataRoot, zipFileName)

download.file(url=surveyURL, destfile=localZipFile)
localDbFile <- unzip(zipfile=localZipFile, exdir=thisDataRoot)

##########################################################################



.... Generate smaller data set to be saved ....







##########################################################################
## Eventually... Disconnect from database, delete database, and save csv files:

DBI::dbDisconnect(conn=con)

unlink(localZipFile)
unlink(localDbFile)

write.csv(..., file.path(thisDataRoot,"....csv"), row.names=FALSE)


