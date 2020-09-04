## Data retrieval script for CDFW's STN Survey.

library(tidyverse)

thisDataRoot <- file.path(data_root,"STN")

## URL for STN Survey Delta Smelt index page:
thisURL <- "http://www.dfg.ca.gov/delta/data/townet/indices.asp?species=3"

##########################################################################
## Retrieve STN Survey Delta Smelt index table:

rawHTML <- xml2::read_html(thisURL)

dsmIndexDf <- rawHTML %>%
	rvest::html_node("table") %>%
	rvest::html_table()

names(dsmIndexDf) <- c("Year","Index")	
dsmIndexDf

write.csv(x=dsmIndexDf, file=file.path(thisDataRoot,"STN_DSM_indices.csv"), 
					row.names=FALSE)

