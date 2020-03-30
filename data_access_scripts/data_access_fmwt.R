#script to download fall-midwater-trawl data and get it ready to use.

library(readxl)
library(tidyverse)

#upload the station lookup table to help calculate the index
stas = read.csv("data/FMWTstations.csv")
stas$Station = as.character(stas$Station)

#Now donload the data. It's an excel file in a zipped foler
#so first we have to donload the zip folder and extract the file
temp <- tempfile()
download.file("ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%201967-2019%20Catch%20Matrix_updated.zip",temp)
foo = unzip(temp, "FMWT 1967-2019 Catch Matrix_updated.xlsx")
FMWT <- read_excel(foo)
unlink(temp)

#subset just fish and environmental variables we care about
FMWT = select(FMWT, Year, Date, Survey, Station, Index, 
              `American Shad`, `Delta Smelt`, `Longfin Smelt`, `Striped Bass age-0`)

#Go from wide to long
FMWTl = pivot_longer(FMWT, cols = c(`American Shad`, `Delta Smelt`, 
                                    `Longfin Smelt`, `Striped Bass age-0`),
                     names_to = "Species", values_to = "catch")

#add areas and weights and month
FMWTl = left_join(FMWTl, stas, by = "Station") %>%
  mutate(Month = month(Date))

#index calculation
FMWTli = filter(FMWTl, !is.na(Area), Survey %in% c(3,4,5,6)) %>%
  group_by(Year, Month, Area, Weight, Species) %>%
  summarize(mcatch = mean(catch))%>%
mutate(wcatch = mcatch*Weight) %>%
  ungroup()

#add them all up for the monthly index
FMWTim = group_by(FMWTli, Year, Month, Species) %>%
  summarize(Index = round(sum(wcatch)))


#add them all up for the annual index
fmwt = group_by(FMWTim, Year, Species) %>%
  summarize(Index = sum(Index))

#1976 was weird, so I'll just put those indexes in by hand

fmwt$Index[which(fmwt$Year==1976 & fmwt$Species=="Delta Smelt")] = 359
fmwt$Index[which(fmwt$Year==1976 & fmwt$Species=="Longfin Smelt")] = 658
fmwt$Index[which(fmwt$Year==1976 & fmwt$Species=="Striped Bass age-0")] = 4548
fmwt$Index[which(fmwt$Year==1976 & fmwt$Species=="American Shad")] = 346


write.csv(fmwt, "data/fmwt.csv", row.names = F)
