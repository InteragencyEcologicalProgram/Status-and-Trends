## Driver Script to connect to the status and trends postgres db

library(ggplot2)
library(lubridate)
library(dplyr)
library(dbplyr)
library(odbc)
library(DBI)
library(tidyverse)

# Setup db connection and connect:
dbname <- "strends"
usr <- "usr"
pw <- "pass"

## Connect to local PostgreSQL via dplyr.
# https://www.rdocumentation.org/packages/dplyr/versions/0.5.0/topics/src_postgres
con <- dplyr::src_postgres(dbname = dbname,
                            host = "192.168.99.100",
                            port = 5432,
                            user = usr,
                            password = pw)
# Delete the pw from memory for security reasons:
rm(pw)


## List all the tables:
# dplyr::src_tbls(con)


## Get SKT data. Using the same volume calculations as a query that Lauren Damon 
## sent me some years ago.
skt_sample <- dplyr::tbl(con,"skt_tblsample")
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
  ungroup()

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

## SKT's methods were standardized by 2004. I think that's why they don't calculate 
## an index prior to that.
skt_index_df$Index[skt_index_df$Year <= 2003] <- NA
skt_index_df


skt_dsm_fig <- ggplot(skt_index_df, aes(x=Year, y=Index))+
  geom_bar(stat="identity",fill="#095E49") +
  theme_iep() +
  theme(legend.position="none") + 
  scale_y_continuous(expression(paste("Delta Smelt Index (fish/m"^"3"*" x 10,000)")))

ggsave(skt_dsm_fig, file="skt_dsm_fig.png", path=smelt_fig_root, 
       dpi=300, units="cm", width=9.3, height=6.8)

