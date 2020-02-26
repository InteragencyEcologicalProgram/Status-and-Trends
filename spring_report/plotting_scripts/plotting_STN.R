
library(smonitr)

projectRoot <- "."
reportRoot <- file.path(projectRoot,"summer_report")
dataRoot <- file.path(projectRoot,"data")
thisDataRoot <- file.path(dataRoot,"STN")
figRoot <- file.path(reportRoot,"figures")

localFile <- file.path(thisDataRoot,"STN_DSM_indices.csv")

source(file.path(projectRoot, "IEP_Plot_Theme.R"))

##########################################################################

dsmIndexDf <- read.csv(localFile, stringsAsFactors=FALSE)
dsmIndexDf

##########################################################################
## Delta Smelt:

dsm_fig <- ggplot(dsmIndexDf, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Index"))) + 
  std_x_axis_all_years(2018) +
  geom_hline(yintercept=mean(dsmIndexDf$Index, na.rm=TRUE), col="red", 
             linetype="dashed", size=0.9)

ggsave(dsm_fig, file=file.path(figRoot,"STN_DSM.png"), dpi=300, units="cm", 
			 width=9.3, height=6.8)
