#IEP Status and Trends Report
#Fall and winter Season
#Data up to and including 2018
#Water quality: temperature, nutrients, fluorescence

#Created by Nick Rasmussen
#edited by Rosemary Hartman
#last updated: 11/6/2019

#geographic regions 
#San Pablo: West of Carquinez straight
#Suisun: Carquinez straight to Collinsville
#Delta: East of Collinsville 
#keep floating stations (EZ stations) separate from fixed stations

#packages
library(ggplot2)
library(zoo)  # yearmon and yearqtr classes
library(tidyr) #separate one column into two
library(dplyr) #count()
library(tidyverse)
library(cowplot) #grid_plot()
library(lubridate)
library(smonitr)


source("data_access_scripts/WQ_data_download.R")
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
alldata$date<-as.POSIXct(alldata$SampleDate, format="%Y-%m-%d %H:%M:%S")
#looks like it figured out that I wanted the time removed

#convert value to numeric
#this didn't happen automatically because of the text for values below detection limit
alldata$value<-as.numeric(as.character(alldata$Result))
#cases where value is below detection limit (ie, < R.L.) become NAs


#reduce AnalyteName list to just those of interest: 
#Field Water Temperature, Chlorophyll a, Field Secchi Depth, microcystis, Specific conductance

alldat<-filter(alldata, AnalyteName %in% c( "Temperature" , "Secchi",  "Microcystis","SpCndSurface",
                                            "WTSurface",
                                         "Temperature" , "Secchi Depth", "Chlorophyll a", "Chla"))



#rename AnalyteNames with simpler names
params = data.frame(AnalyteName = c("Temperature" , "Secchi",  "Microcystis","SpCndSurface",
                                    "WTSurface",
                                    "Temperature" , "Secchi Depth", "Chlorophyll a", "Chla"),
paramsshort = c("temp", "secchi","Microcystis","cond", "temp", "temp" , "secchi", "chla", "chla"))

alldat = left_join(alldat, params, by = "AnalyteName") %>%
  mutate(AnalyteName = paramsshort) %>%
  mutate(paramsshort = NULL)


#add lat and long to alldat
salldat<-left_join(alldat, siteloc)

#look at combination of site and id and look at how many cases there are of each combo
sites2<-table(salldat$id, salldat$StationCode)
#there is a lot of variation in the number of observations per combo which is reasonable

#remove rows in which site is NA (ie, those that weren't in the file with lat and long)
tot<-salldat[!is.na(salldat$lat),]


#create a month, year, and season column
#the "+ 1/12" makes sure that December ends up as the first month of Q1 instead of Jan
tot<- mutate(tot, month = month(tot$date), year = year(tot$date),
             ym = as.yearmon(paste(month, year), "%m %Y"),
             yq = as.yearqtr(ym + 1/12), yq2 = yq) %>% 
  separate(yq2, c('qyear', 'quarter'), sep=" ")


#make quarter a factor
tot$quarter<-factor(tot$quarter)

#add column for geographic region based on longitude
tot$region<-factor(ifelse(tot$long < -122.216, "spl", 
                    ifelse(tot$long > -122.216 & tot$long < -121.829, "ss",
                           ifelse(tot$long > -121.829, "dt",NA))) )


foo = group_by(tot, region, year) %>%
  summarize(stas = length(unique(StationCode)))

#generate means by region, year, quarter, and AnalyteName
wqsum<-aggregate(value~region+qyear+quarter+AnalyteName,data=tot,FUN=mean,na.rm=T)
wqsum$AnalyteName<-as.factor(wqsum$AnalyteName)
str(wqsum)


#generate means by region, quarter, and AnalyteName
wqsum2<-aggregate(value~region+quarter+AnalyteName,data=tot,FUN=mean,na.rm=T)
wqsum2$AnalyteName<-as.factor(wqsum2$AnalyteName)
str(wqsum2)


#create custom plot formatting function
#source("winter_report/IEP_Status&Trends_util.R")

#set up facet labels
season_names<-c('Q1'="winter",'Q2'="spring",'Q3'="summer",'Q4'="fall")
region_names<-c('dt'="Delta",'ss'="Suisun",'spl'="San Pablo")
AnalyteName_labs = c("chlf" = "Chlorophyll Fluoresence (RFU)", 
                   "chla" = "Chlorophyll-a (ug/L)",
                   "temp" = "Temperature (C)",
                   "ammonia" = "Ammonium (mg/L)",
                   "nit" = "Dissolved Nitrate + Nitrite (mg/L)",
                   "secchi" = "Secchi Depth (cm)")

#set order of seasons and regions for plotting
wqsum$quarter = factor(wqsum$quarter, levels=c('Q1','Q2','Q3','Q4'))

wqsum$region = factor(wqsum$region, levels=c('spl','ss','dt'))


#change year from chr to int
wqsum$qyear<-as.integer(wqsum$qyear)

 
#Create facet of plots for each AnalyteName showing all combinations of region and season

allplots = function(data, param, reportyear){
  dat = filter(data, AnalyteName == param, qyear <= reportyear)
  p <- ggplot(dat, aes(x=qyear, y=value))+
    geom_line(colour="black")+geom_point(colour="black") +
    geom_hline(data = filter(wqsum2, AnalyteName == param),
               aes(yintercept = value), size = 0.9, color = "red", linetype = "dashed")+
    smr_theme() + facet_grid(quarter~region,
                             labeller = as_labeller(
                               c(region_names,season_names))) +
    theme(legend.position="none") + 
    scale_y_continuous(name = AnalyteName_labs[param],limits=c(0, max(dat$value)))+
    scale_x_continuous("Year", limits=c(1966,2018))
  return(p)
}

allplots(wqsum, "chla", 2018)
allplots(wqsum, "chlf", 2018)
allplots(wqsum, "temp", 2018)
allplots(wqsum, "secchi", 2018)


#now, for fall, make a plot for each region, season, and AnalyteName separately
#do this so you can patch them together as grobs (like fish data panel)

#first a function
WQplot = function(reg, quart, analyte, data, reportyear) {
  #filter the dataset based on season, analyte, and region
  dat = filter(data, quarter == quart, region ==reg, AnalyteName == analyte, qyear <= reportyear)
  
  #set up limits for plot based on the max and min for all years and regions
  dat2 = filter(data, AnalyteName == analyte)
  lims = c(min(dat2$value), max(dat2$value))
    
    #make the plot
  p_sec <- ggplot(dat, aes(x=qyear, y= value))+
    geom_line(colour="black", size = 0.9)+geom_point(colour="black", size = 1.6) +
    geom_hline(aes(yintercept = mean(value)), size = 0.9, color = "red", linetype = "dashed")+
    smr_theme() + 
    theme(legend.position="none") + 
    smr_x_axis(2018, "all", season = )+
    scale_y_continuous(AnalyteName_labs[analyte] , limits=lims)+
    std_x_axis_label(season_names[quart])
    
   return(p_sec)
  }
  
 

#function to plot all graphs for a particular season and analyte seperately
plotall = function(quart, analyte, data, report_year) {
  
  #plot for each region for the season
  spl =WQplot("spl", quart, analyte, data, report_year)
  ss = WQplot("ss", quart, analyte, data, report_year)
  dt = WQplot("dt", quart, analyte, data, report_year)
  
  #grob them together into a single pannel
  tmps<-plot_grid(spl, ss, dt,
                  ncol = 3, nrow = 1, align="v")
  
  #save them together and seperately
  ggsave(tmps, file=paste(analyte, "_panel_", season_names[quart], ".png", sep = ""), dpi=300, units="cm",width=27.9,height=6.8,
         path = paste("./", season_names[quart], "_report/figures", sep = ""))
  
  ggsave(spl, file=paste(analyte, "_spl", season_names[quart], ".png", sep = ""), dpi=300, units="cm",width=9.3,height=6.8,
         path = paste("./", season_names[quart], "_report/figures", sep = ""))
  ggsave(ss, file=paste(analyte, "_ss", season_names[quart], ".png", sep = ""), dpi=300, units="cm",width=9.3,height=6.8,
         path = paste("./", season_names[quart], "_report/figures", sep = ""))
  ggsave(dt, file=paste(analyte, "_dt", season_names[quart], ".png", sep = ""), dpi=300, units="cm",width=9.3,height=6.8,
         path = paste("./", season_names[quart], "_report/figures", sep = ""))
  
}

#Now get all of them for all the seasons!
plotallseason = function(analyte, data, report_year){
  plotall("Q1", analyte, data, report_year)
  plotall("Q2", analyte, data, report_year)
  plotall("Q3", analyte, data, report_year)
  plotall("Q4", analyte, data, report_year)
}

#crank out the plots
plotallseason("temp", wqsum, 2018)
plotallseason("secchi", wqsum, 2018)
plotallseason("chla", wqsum, 2018)
