#Download the WQ data and get it in the right format

library(readxl)
library(curl)
library(tidyverse)
library(readr)
library(readxl)


#read in all the excel files. It's 10 years at a time
#And field data and lab data are seperate.

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

url <- "https://emp.baydeltalive.com/assets/e106ca2a359a122e74e33ef183a0fb4a/text/csv/SACSJ_delta_water_quality_2000_2018.csv"
destfile <- "Field_Data_2000_2018x.csv"
curl_download(url, destfile)


#Teh 2000-2018 data is in a totally different format, which is
#annoying. They are working on updating the data and moving it
#all to EDI, but they are starting with the most recent data
#and moving backwards. 

Field_Data_2000_2018x <- read_csv("Field_Data_2000_2018x.csv", 
                                  col_types = cols(Chla = col_number(), DissAmmonia = col_number(),
                                                   DOBottom = col_number(), DON = col_character(), 
                                                   DOSurface = col_number(), Date = col_date(format = "%m/%d/%y"), 
                                                   Depth = col_number(), FluorescenceSurface = col_number(), 
                                                   Microcystis = col_number(), NorthLat = col_number(), 
                                                   Pheophytin = col_number(), SampleDescription = col_character(), 
                                                   Secchi = col_number(), Time = col_time(format = "%H:%M"), 
                                                   TurbidityBottom = col_number(), TurbiditySurface = col_number(), 
                                                   WTBottom = col_number(), WestLong = col_number(), 
                                                   pHBottom = col_number()))

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

