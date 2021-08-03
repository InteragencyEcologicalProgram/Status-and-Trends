## Data retrieval script for CDFW's SKT Survey.

library(dplyr)
library(RODBC)
library(DBI)
library(odbc)

##########################################################################

thisDataRoot <- file.path(data_root,"SKT")

## SKT Survey url:
surveyURL <- "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SKT.zip"

## Name of SKT Survey zip file: 
zipFileName <- "SKT.zip"

## Name of SKT Survey database within the zip file:
dbName <- "SKT.accdb"

##########################################################################
## Retrieve SKT Survey database copy:

## Download and unzip the file:
localZipFile <- file.path(thisDataRoot,zipFileName)

download.file(url=surveyURL, destfile=localZipFile)
localDbFile <- unzip(zipfile=localZipFile, exdir=thisDataRoot)

##########################################################################
## Retrieve SKT data:

## Using the same volume calculations as a query that Lauren Damon sent me 
## some years ago.
dbString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
									 "Dbq=",localDbFile)
con <- DBI::dbConnect(drv=odbc::odbc(), .connection_string=dbString)
DBI::dbListTables(con)

skt_sample <- DBI::dbReadTable(con,"tblsample")
skt_catch <- DBI::dbReadTable(con,"tblcatch")
skt_organism <- DBI::dbReadTable(con,"tblorganismcodes")
skt_stn <- DBI::dbReadTable(con,"lktblstationsskt")

## Disconnect from database:
DBI::dbDisconnect(conn=con)

sktData <- dplyr::left_join(skt_catch, skt_organism, by="OrganismCode")
sktData <- dplyr::inner_join(skt_sample, sktData, by="SampleRowID")


##########################################################################
## Save reduced data files:

write.csv(sktData, file.path(thisDataRoot,"sktData.csv"), row.names=FALSE)


##########################################################################
## Remove original files:

unlink(localZipFile)
unlink(localDbFile)
















# #figure out how to connect to an access database

# access_query_32 <- function(db_table = "qryData_RM", table_out = "data_access") {
  # library(svSocket)
  
  # # variables to make values uniform
  # sock_port <- 8642L
  # sock_con <- "sv_con"
  # ODBC_con <- "a32_con"
  # db_path <- "data/SKT/SKT.accdb"
  
  # if (file.exists(db_path)) {
    
    # # build ODBC string
    # ODBC_str <- local({
      # s <- list()
      # s$path <- paste0("DBQ=", gsub("(/|\\\\)+", "/", path.expand(db_path)))
      # s$driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
      # s$threads <- "Threads=4"
      # s$buffer <- "MaxBufferSize=4096"
      # s$timeout <- "PageTimeout=5"
      # paste(s, collapse=";")
    # })
    
    # # start socket server to transfer data to 32 bit session
    # startSocketServer(port=sock_port, server.name="access_query_32", local=TRUE)
    
    # # build expression to pass to 32 bit R session
    # expr <- "library(svSocket)"
    # expr <- c(expr, "library(RODBC)")
    # expr <- c(expr, sprintf("%s <- odbcDriverConnect('%s')", ODBC_con, ODBC_str))
    # expr <- c(expr, sprintf("if('%1$s' %%in%% sqlTables(%2$s)$TABLE_NAME) {%1$s <- sqlFetch(%2$s, '%1$s')} else {
                            # %1$s <- 'table %1$s not found'}", db_table, ODBC_con))
    # expr <- c(expr, sprintf("%s <- socketConnection(port=%i)", sock_con, sock_port))
    # expr <- c(expr, sprintf("evalServer(%s, %s, %s)", sock_con, table_out, db_table))
    # expr <- c(expr, "odbcCloseAll()")
    # expr <- c(expr, sprintf("close(%s)", sock_con))
    # expr <- paste(expr, collapse=";")
    
    # # launch 32 bit R session and run expressions
    # prog <- file.path(R.home(), "bin", "i386", "Rscript.exe")
    # system2(prog, args=c("-e", shQuote(expr)), stdout=NULL, wait=TRUE, invisible=TRUE)
    
    # # stop socket server
    # stopSocketServer(port=sock_port)
    
    # # display table fields
    # message("retrieved: ", table_out, " - ", paste(colnames(get(table_out)), collapse=", "))
  # } else {
    # warning("database not found: ", db_path)
  # }
# }
# skt_sample <- access_query_32(db_table ="skt_tblsample")