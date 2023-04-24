#Download the WQ data and get it in the right format

source("setup.R")

library(readxl)
library(curl)
library(readr)
library(readxl)
library(lubridate)

#all of EMP's data is now on EDI

EMP = get_edi_data(458, "EMP_DWQ_1975_2022", guess_max = 1000000) 

EMP = EMP$EMP_DWQ_1975_2022
 

#Switch the field data from wide to long.

EMP2 = select(EMP, Station, Date, Chla, WTSurface, Secchi, Microcystis) %>%
  mutate(Chla = as.numeric(Chla)) %>%
  rename(chla = Chla, temp = WTSurface, secchi = Secchi) %>%
  pivot_longer(cols = c(chla, temp, secchi, Microcystis), names_to = "AnalyteName", values_to = "Result") 



#save the result
write.csv(EMP2, file.path(data_root,"WQ_Discrete_1975-2022.csv"), row.names = F)

