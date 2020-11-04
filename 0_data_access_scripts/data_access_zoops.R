#script to download zooplankton data and get it ready to use.

library(readxl)
library(tidyverse)
library(smonitr)


#data is stored on the CDFW FTP site
CBzoop = get_ftp_data(ftp_address = "ftp://ftp.wildlife.ca.gov", 
                      dir_path = "IEP_Zooplankton", 
                    "CBMatrix", parse_remote_excel, 
                    sheet = "CB CPUE Matrix 1972-2019",
                    guess_max = 100000L)
zoopcb = CBzoop$`1972-2019CBMatrix.xlsx`

zoopcb$Date = as.Date(zoopcb$SampleDate)
zoopcb = rename(zoopcb, Station = StationNZ)

#pump data
zoopp<- get_ftp_data(ftp_address = "ftp://ftp.wildlife.ca.gov", 
                     dir_path = "IEP_Zooplankton", 
                     "PumpMatrix", parse_remote_excel, 
                     sheet = "Pump CPUE Matrix 1972-2019",
                     guess_max = 100000L)
zoopp = zoopp$`1972-2019PumpMatrix.xlsx`
zoopp$Date = as.Date(zoopp$SampleDate)


