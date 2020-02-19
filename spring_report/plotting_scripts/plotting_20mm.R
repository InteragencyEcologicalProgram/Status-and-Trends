
library(ggplot2)

projectRoot <- "."
reportRoot <- file.path(projectRoot,"spring_report")
dataRoot <- file.path(projectRoot,"data")
thisDataRoot <- file.path(dataRoot,"20mm")
figRoot <- file.path(reportRoot,"figures")

source(file.path(projectRoot, "IEP_Plot_Theme.R"))

##########################################################################
## Read in index data:

dsmIndexDf <- read.csv(file.path(thisDataRoot,"20mm_DSM_index.csv"),
											 stringsAsFactors=FALSE)
lfsIndexDf <- read.csv(file.path(thisDataRoot,"20mm_LFS_index.csv"), 
											 stringsAsFactors=FALSE)


##########################################################################
## Delta Smelt

dsm_fig <- ggplot(dsmIndexDf, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
  theme_iep() +
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  geom_hline(yintercept=mean(dsmIndexDf$Index, na.rm=TRUE), col="red", 
             linetype="dashed", size=0.9)

ggsave(dsm_fig, file=file.path(figRoot,"20mm_DSM.png"), dpi=300, units="cm", 
			 width=9.3, height=6.8)

			 
##########################################################################
## Longfin Smelt

lfs_fig <- ggplot(lfsIndexDf, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
  theme_iep() +
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  geom_hline(yintercept=mean(lfsIndexDf$Index, na.rm=TRUE), col="red", 
             linetype="dashed", size=0.9)

ggsave(lfs_fig, file=file.path(figRoot,"20mm_LFS.png"), dpi=300, units="cm", 
			 width=9.3, height=6.8)


