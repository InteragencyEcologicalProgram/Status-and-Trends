## Data retrieval script for CDFW's STN Survey.

source("setup.R")

##########################################################################

thisDataRoot <- file.path(data_root,"STN")

dsmIndexDf = read_csv("data/STN/STN_smeltindex.csv")

##########################################################################
## Save data file:

write.csv(x=dsmIndexDf, file=file.path(thisDataRoot,"STN_DSM_indices.csv"), 
					row.names=FALSE)

