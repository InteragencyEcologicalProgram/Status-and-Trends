#temperature heat map


#packages
library(ggplot2)
library(zoo)  # yearmon and yearqtr classes
library(tidyr) #separate one column into two
library(dplyr) #count()
library(tidyverse)
library(cowplot) #grid_plot()
library(lubridate)
library(smonitr)


#source("data_access_scripts/WQ_data_download.R")
#or skip this if you've updated it recently and just do
alldata = read.csv("data/WQ_discrete_1975-2018.csv", stringsAsFactors = F)
alldata$SampleDate = as.Date(alldata$SampleDate)

#EMP WQ data (1975-2018)
#alldata<- WQ_all
str(alldata)

#GPS coordinates of all sites
siteloc<-read.csv("./data/wq_stations.csv") 
siteloc = mutate(siteloc, StationCode = site)

#format date column so R recognizes it as such
#alldata$date<-as.POSIXct(alldata$SampleDate, format="%Y-%m-%d %H:%M:%S")
#looks like it figured out that I wanted the time removed

#convert value to numeric
#this didn't happen automatically because of the text for values below detection limit
alldata$value<-as.numeric(as.character(alldata$Result))
#cases where value is below detection limit (ie, < R.L.) become NAs


#reduce AnalyteName list to just those of interest: 
#Field Water Temperature, Chlorophyll a, Field Secchi Depth, microcystis, Specific conductance

Temp<-filter(alldata, AnalyteName %in% c( "Temperature" , 
                                            "WTSurface"))

#add lat and long to alldat
salldat<-left_join(Temp, siteloc)

#remove rows in which site is NA (ie, those that weren't in the file with lat and long)
tot<-salldat[!is.na(salldat$lat),]


#create a month, year
tot<- mutate(tot, month = month(tot$SampleDat), year = year(tot$SampleDate))

#add column for geographic region based on longitude
tot$region<-factor(ifelse(tot$long < -122.216, "spl", 
                          ifelse(tot$long > -122.216 & tot$long < -121.829, "ss",
                                 ifelse(tot$long > -121.829, "dt",NA))) )

tot = filter(tot, region == "dt")

#generate means by region, year, quarter, and AnalyteName
tempsum<-aggregate(value~region+year+month,data=tot,FUN=mean,na.rm=T)

tempsum = tempsum[order(tempsum$value),]

tempsum = mutate(tempsum, 
                 month = factor(month, labels = c("Jan", "Feb", "Mar",
                                                  "Apr","May","Jun", "Jul","Aug",
                                                  "Sep","Oct","Nov","Dec")))

tempsum = group_by(tempsum, month) %>% mutate(Rank = rank(value)) %>% ungroup()
#simple heat map
ggplot(data = tempsum, aes(x = year, y = month)) +
  geom_tile(aes(fill = value))+
  scale_fill_gradient(low = "green", high = "red")

  #heat map by rank  
  ggplot(data = tempsum, aes(x = year, y = month)) +
           geom_tile(aes(fill = Rank)) +
    scale_fill_gradient(low = "green", high = "red")+
    geom_text(aes(label = Rank))
         
