#IEP Status and Trends Report
#Fall and winter Season
#Data up to and including 2018
#Water quality: temperature, nutrients, fluorescence

#Created by Nick Rasmussen
#edited by Rosemary Hartman
#last updated: 11/3/2020

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
library(cowplot) #grid_plot()
library(lubridate)
library(smonitr)


source(file.path(data_access_root,"WQ_data_download.R"))
#or skip this if you've updated it recently and just do
alldata = read.csv(file.path(data_root,"WQ_discrete_1975-2019.csv"), stringsAsFactors = F)
alldata$Date = mdy(alldata$Date)

#EMP WQ data (1975-2019)
#alldata<- WQ_all
  str(alldata)

#GPS coordinates of all sites
siteloc<-read.csv(file.path(data_root,"wq_stations.csv")) 
siteloc = mutate(siteloc, Station = site)

#add lat and long to alldat
salldat<-left_join(alldata, siteloc)

#remove rows in which site is NA (ie, those that weren't in the file with lat and long)
tot<-salldat[!is.na(salldat$lat),]


#create a month, year, and season column
#the "+ 1/12" makes sure that December ends up as the first month of Q1 instead of Jan
tot<- mutate(tot, month = month(tot$Date), year = year(tot$Date),
             ym = as.yearmon(paste(month, year), "%m %Y"),
             yq = as.yearqtr(ym + 1/12), yq2 = yq) %>% 
  separate(yq2, c('qyear', 'quarter'), sep=" ")


#make quarter a factor
tot$quarter<-factor(tot$quarter)

#add column for geographic region based on longitude
tot$region<-factor(ifelse(tot$long < -122.216, "spl", 
                    ifelse(tot$long > -122.216 & tot$long < -121.829, "ss",
                           ifelse(tot$long > -121.829, "dt",NA))) )

#generate means by region, year, quarter, and AnalyteName
wqsum<-aggregate(Result~region+qyear+quarter+AnalyteName,data=tot,FUN=mean,na.rm=T)
wqsum$AnalyteName<-as.factor(wqsum$AnalyteName)
str(wqsum)


#generate means by region, quarter, and AnalyteName
wqsum2<-aggregate(Result~region+quarter+AnalyteName,data=tot,FUN=mean,na.rm=T)
wqsum2$AnalyteName<-as.factor(wqsum2$AnalyteName)
str(wqsum2)


#create custom plot formatting function

#set up facet labels
season_names<-c('Q1'="winter",'Q2'="spring",'Q3'="summer",'Q4'="fall")
region_names<-c('dt'="Delta",'ss'="Suisun",'spl'="San Pablo")
AnalyteName_labs = c( "chla" = "Chlorophyll-a (ug/L)",
                   "temp" = "Temperature (C)",
                   "secchi" = "Secchi Depth (cm)")

#set order of seasons and regions for plotting
wqsum$quarter = factor(wqsum$quarter, levels=c('Q1','Q2','Q3','Q4'))

wqsum$region = factor(wqsum$region, levels=c('spl','ss','dt'))


#change year from chr to int
wqsum$qyear<-as.integer(wqsum$qyear)

 
#Create facet of plots for each AnalyteName showing all combinations of region and season

allplots = function(data, param, reportyear){
  dat = filter(data, AnalyteName == param, qyear <= reportyear)
  p <- ggplot(dat, aes(x=qyear, y=Result))+
    geom_line(colour="black")+geom_point(colour="black") +
    geom_hline(data = filter(wqsum2, AnalyteName == param),
               aes(yintercept = Result), size = 0.9, color = "red", linetype = "dashed")+
    smr_theme() + facet_grid(quarter~region,
                             labeller = as_labeller(
                               c(region_names,season_names))) +
    theme(legend.position="none") + 
    scale_y_continuous(name = AnalyteName_labs[param],limits=c(0, max(dat$Result)))+
    smr_x_axis(reportyear, type = "all", season = season_names[quarter])
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
  lims = c(min(dat2$Result), max(dat2$Result))
    
    #make the plot
  p_sec <- ggplot(dat, aes(x=qyear, y= Result))+
    geom_line(colour="black", size = 0.9)+geom_point(colour="black", size = 1.6) +
    geom_hline(aes(yintercept = mean(Result)), size = 0.9, color = "red", linetype = "dashed")+
    smr_theme() + 
    theme(legend.position="none") + 
    smr_x_axis(reportyear, type = "all", season = season_names[quart])+
    scale_y_continuous(AnalyteName_labs[analyte] , limits=lims)
    
   return(p_sec)
  }
  
WQplot("dt","Q1", "chla", wqsum, report_year) 

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
         path = file.path(fig_root, season_names[quart]))
  
  ggsave(spl, file=paste(analyte, "_spl", season_names[quart], ".png", sep = ""), dpi=300, units="cm",width=9.3,height=6.8,
         path = file.path(fig_root, season_names[quart]))
  ggsave(ss, file=paste(analyte, "_ss", season_names[quart], ".png", sep = ""), dpi=300, units="cm",width=9.3,height=6.8,
         path = file.path(fig_root, season_names[quart]))
  ggsave(dt, file=paste(analyte, "_dt", season_names[quart], ".png", sep = ""), dpi=300, units="cm",width=9.3,height=6.8,
         path = file.path(fig_root, season_names[quart]))
  
}

#Now get all of them for all the seasons!
plotallseason = function(analyte, data, report_year){
  plotall("Q1", analyte, data, report_year)
  plotall("Q2", analyte, data, report_year)
  plotall("Q3", analyte, data, report_year)
  plotall("Q4", analyte, data, report_year)
}

#crank out the plots
plotallseason("temp", wqsum, report_year)
plotallseason("secchi", wqsum, report_year)
plotallseason("chla", wqsum, report_year)
