#Download the WQ data and get it in the right format

library(readxl)
library(curl)
library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(smonitr)


#read in all the excel files. It's 10 years at a time
#And field data and lab data are seperate.

EMPField = get_baydeltalive_data("00c870b6fdc0e30d0f92d719984cfb44",
                                 "application/vnd.ms-excel", "Field_Data_[0-9]{4}-[0-9]{4}x*",
                                 parse_remote_excel, guess_max = 100000L)

#Error in .f(.x[[i]], ...) : 
#  https://emp.baydeltalive.com/assets/00c870b6fdc0e30d0f92d719984cfb44/application/vnd.ms-excel/Field_Data_1975-1987x.xlsx does not appear to be a valid Excel file.

url <- "https://emp.baydeltalive.com/assets/00c870b6fdc0e30d0f92d719984cfb44/application/vnd.ms-excel/Lab_Data_1985-1995x.xlsx"
destfile <- "Lab_Data_1985_1995x.xlsx"
curl_download(url, destfile)
Lab_Data_1985_1995x <- read_excel(destfile, 
                                  col_types = c("text", "text", "text", 
                                                "text", "date", "text", "text", "text", 
                                                "numeric", "text", "numeric", "numeric", 
                                                "text"))


url <- "https://emp.baydeltalive.com/assets/00c870b6fdc0e30d0f92d719984cfb44/application/vnd.ms-excel/Lab_Data_1975-1984x.xlsx"
destfile <- "Lab_Data_1975_1984x.xlsx"
curl_download(url, destfile)
Lab_Data_1975_1984x <- read_excel(destfile, 
                                  col_types = c("text", "text", "text", 
                                                "text", "date", "text", "text", "text", 
                                                "numeric", "text", "numeric", "numeric", 
                                                "text"))

url <- "https://emp.baydeltalive.com/assets/00c870b6fdc0e30d0f92d719984cfb44/application/vnd.ms-excel/Lab_Data_1996-2012.xlsx"
destfile <- "Lab_Data_1996_1912x.xlsx"
curl_download(url, destfile)
Lab_Data_1996_2012x <- read_excel(destfile, 
                                  col_types = c("text", "text", "text", 
                                                "text", "date", "text", "text", "text", 
                                                "numeric", "text", "numeric", "numeric", 
                                                "text"))


url <- "https://emp.baydeltalive.com/assets/00c870b6fdc0e30d0f92d719984cfb44/application/vnd.ms-excel/Field_Data_1975-1987x.xlsx"
destfile <- "Field_Data_1975_1987x.xlsx"
curl_download(url, destfile)
Field_Data_1975_1987x <- read_excel(destfile, 
                                    col_types = c("text", "date", "text", 
                                                  "numeric", "text", "text", "text", 
                                                  "text", "numeric", "numeric", "numeric", 
                                                  "text", "text", "text", "text", "text"))


url <- "https://emp.baydeltalive.com/assets/00c870b6fdc0e30d0f92d719984cfb44/application/vnd.ms-excel/Field_Data_1988-2006x.xlsx"
destfile <- "Field_Data_1988_2006x.xlsx"
curl_download(url, destfile)
Field_Data_1988_2006x <- read_excel(destfile, 
                                    col_types = c("text", "date", "text", 
                                                  "numeric", "text", "text", "text", 
                                                  "text", "numeric", "numeric", "numeric", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text"))

#The 2000-2018 data is all on EDI
Field_Data_2000_2018x = get_edi_data(458, "Water quality data from the California Bay-Delta watershed", guess_max = 1000000) 

Field_Data_2000_2018x = Field_Data_2000_2018x$`Water quality data from the California Bay-Delta watershed`

#Teh 2000-2018 data is in a totally different format, which is
#annoying. They are working on updating the data and moving it
#all to EDI, but they are starting with the most recent data
#and moving backwards. 

#Switch the field data from wide to long.

Field = pivot_longer(Field_Data_2000_2018x[,c(1:12, 27:29, 32)], 
                     cols = 10:16, names_to = "AnalyteName", values_to = "Result") %>%
  select(Station, Date, AnalyteName, Result)

#Rename the fields to match
names(Field) = c("StationCode", "SampleDate", "AnalyteName", "Result")

#Bind all the data files together.
FieldData = rbind(Field_Data_1975_1987x, Field_Data_1988_2006x[,-17]) %>%
  filter(SampleDate < "2000-01-01", Matrix == "Water") %>%
  select(SampleDate, StationCode, Result, AnalyteName)

FieldData2 = rbind(Field, FieldData)

LabData = rbind(Lab_Data_1975_1984x, Lab_Data_1985_1995x, Lab_Data_1996_2012x) %>%
  select(SampleDate, StationCode, Result, ConstituentName)
LabData = rename(LabData, AnalyteName = ConstituentName)

WQ_all = rbind(LabData, FieldData2)

#save the result
write.csv(WQ_all, "./data/WQ_Discrete_1975-2018.csv", row.names = F)


            