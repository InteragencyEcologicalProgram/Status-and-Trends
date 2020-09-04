## Data retrieval script for CDFW's SKT Survey.
library(dplyr)
library(RODBC)


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






#figure out how to connect to an access database

access_query_32 <- function(db_table = "qryData_RM", table_out = "data_access") {
  library(svSocket)
  
  # variables to make values uniform
  sock_port <- 8642L
  sock_con <- "sv_con"
  ODBC_con <- "a32_con"
  db_path <- "data/SKT/SKT.accdb"
  
  if (file.exists(db_path)) {
    
    # build ODBC string
    ODBC_str <- local({
      s <- list()
      s$path <- paste0("DBQ=", gsub("(/|\\\\)+", "/", path.expand(db_path)))
      s$driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
      s$threads <- "Threads=4"
      s$buffer <- "MaxBufferSize=4096"
      s$timeout <- "PageTimeout=5"
      paste(s, collapse=";")
    })
    
    # start socket server to transfer data to 32 bit session
    startSocketServer(port=sock_port, server.name="access_query_32", local=TRUE)
    
    # build expression to pass to 32 bit R session
    expr <- "library(svSocket)"
    expr <- c(expr, "library(RODBC)")
    expr <- c(expr, sprintf("%s <- odbcDriverConnect('%s')", ODBC_con, ODBC_str))
    expr <- c(expr, sprintf("if('%1$s' %%in%% sqlTables(%2$s)$TABLE_NAME) {%1$s <- sqlFetch(%2$s, '%1$s')} else {
                            %1$s <- 'table %1$s not found'}", db_table, ODBC_con))
    expr <- c(expr, sprintf("%s <- socketConnection(port=%i)", sock_con, sock_port))
    expr <- c(expr, sprintf("evalServer(%s, %s, %s)", sock_con, table_out, db_table))
    expr <- c(expr, "odbcCloseAll()")
    expr <- c(expr, sprintf("close(%s)", sock_con))
    expr <- paste(expr, collapse=";")
    
    # launch 32 bit R session and run expressions
    prog <- file.path(R.home(), "bin", "i386", "Rscript.exe")
    system2(prog, args=c("-e", shQuote(expr)), stdout=NULL, wait=TRUE, invisible=TRUE)
    
    # stop socket server
    stopSocketServer(port=sock_port)
    
    # display table fields
    message("retrieved: ", table_out, " - ", paste(colnames(get(table_out)), collapse=", "))
  } else {
    warning("database not found: ", db_path)
  }
}




## Get SKT data. Using the same volume calculations as a query that Lauren Damon 
## sent me some years ago.
skt_sample <- access_query_32(db_table ="skt_tblsample")
skt_catch <- dplyr::tbl(con,"skt_tblcatch")
skt_organism <- dplyr::tbl(con,"skt_tblorganismcodes")
skt_stn <- dplyr::tbl(con,"skt_lktblstationsskt")

skt_catch <- dplyr::left_join(skt_catch, skt_organism, by="OrganismCode")
skt_catch <- dplyr::inner_join(skt_sample, skt_catch, by="SampleRowID")

skt_catch <- data.frame(skt_catch)
skt_catch$SampleDate <- as.Date(as.character(skt_catch$SampleDate))
skt_catch$SampleTimeStart <- as.character(skt_catch$SampleTimeStart)
skt_catch$SampleTimeEnd <- as.character(skt_catch$SampleTimeEnd)
skt_catch$Month <- lubridate::month(skt_catch$SampleDate)
skt_catch$Year <- lubridate::year(skt_catch$SampleDate)
skt_catch$MeterCounts <- with(skt_catch, ifelse(MeterEnd < MeterStart, 
                                                ((1000000-MeterStart) + MeterEnd), 
                                                (MeterEnd - MeterStart)))
skt_catch$Volume_cubicm <- skt_catch$MeterCounts*0.02687*13.95



catch_mat <- skt_catch %>%
  group_by(Year, Month, SurveyNumber, SampleDate, StationCode,
           SampleTimeStart, SampleTimeEnd, Volume_cubicm) %>%
  summarize(DeltaSmelt=sum(Catch[CommonName == "delta smelt"])) %>%
  ungroup() %>%
  filter(Year <= 2017)

## Recreate SKT index (see "MEMO2015 SKT Delta Smelt Index.pdf" for instructions 
## on how to define the regions and calculate the index):
region_map <- list("Confluence and West"=c("340","405","411","418","501","504","508",
                                           "513","519","520","602","606","609","610",
                                           "801"),
                   "Sacramento River System"=c("704","706","707","711","712","713",
                                               "715","716","724"),
                   "San Joaquin River System"=c("804","809","812","815","902","906",
                                                "910","912","914","915","919","920",
                                                "921","922","923"))
region_map_inverse <- gsub("[0-9]+","",names(unlist(region_map)))
names(region_map_inverse) <- unlist(region_map)

## Keep the same surveys and use the same regions as CDFW:
catch_mat_sub <- catch_mat %>%
  filter(as.character(StationCode) %in% names(region_map_inverse) & 
           SurveyNumber %in% 1:4) %>%
  mutate(Region=region_map_inverse[as.character(StationCode)])

## Calculate the index:
skt_index_df <- catch_mat_sub %>%
  group_by(Year, SurveyNumber, Region, StationCode) %>%
  summarize(Dens=sum(DeltaSmelt)/sum(Volume_cubicm)) %>%
  ungroup() %>%
  group_by(Year, SurveyNumber, Region) %>%
  summarize(Dens=mean(Dens)) %>%
  ungroup() %>%
  group_by(Year) %>%
  summarize(Index=10000*sum(Dens)) %>%
  ungroup()







##########################################################################
## Eventually... Disconnect from database, delete database, and save csv files:

DBI::dbDisconnect(conn=con)

unlink(localZipFile)
unlink(localDbFile)

write.csv(..., file.path(thisDataRoot,"....csv"), row.names=FALSE)


