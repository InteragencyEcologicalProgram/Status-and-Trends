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


source("drivr_sqlite.R")
#EMP WQ data (1975-2017)
alldata<- WQ_all
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


#reduce AnalyteName list to just those of interest
#Total Ammonia, Field Water Temperature, Chlorophyll a, Dissolved Nitrate + Nitrite,
#Dissolved Ammonia, Field Fluorescence, Field Secchi Depth, Field Turbidity

alldat<-filter(alldata, AnalyteName %in% c("Ammonia (Total)" ,"Ammonia (Dissolved)" , "Temperature" ,
                                         "Chlorophyll a", "Nitrite + Nitrate (Dissolved)",
                                         "Dissolved Ammonia", "Fluorescence" ,
                                         "Temperature" , "Secchi Depth",
                                         "Turbidity" ))


#NOTE: Dissolved Ammonia and Total Ammonia are the dame thing but names
# have changed over time. Field Temperature is only used as a AnalyteName name for 
#one observation, which isn't
#repesented in Field Water Temperature, so combine these two categories

#rename AnalyteNames with simpler names
params = data.frame(AnalyteName = c("Ammonia (Total)" ,"Ammonia (Dissolved)" , 
                                    "Temperature" ,
                                    "Chlorophyll a", "Nitrite + Nitrate (Dissolved)",
                                     "Fluorescence" ,
                                     "Secchi Depth",
                                    "Turbidity"),
paramsshort = c("ammonia", "ammonia","temp", "chla", "nitro", "chlf",  "secchi", "turb" ))

alldat = left_join(alldat, params, by = "AnalyteName") %>%
  mutate(AnalyteName = paramsshort) %>%
  mutate(paramsshort = NULL)

#remove secchi depth observations that aren't in cm
allda<-subset(alldat,UnitName !="Feet" & UnitName!="Meters")



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


#generate means by region, year, quarter, and AnalyteName
wqsum<-aggregate(value~region+qyear+quarter+AnalyteName,data=tot,FUN=mean,na.rm=T)
wqsum$AnalyteName<-as.factor(wqsum$AnalyteName)
str(wqsum)


#generate means by region, quarter, and AnalyteName
wqsum2<-aggregate(value~region+quarter+AnalyteName,data=tot,FUN=mean,na.rm=T)
wqsum2$AnalyteName<-as.factor(wqsum2$AnalyteName)
str(wqsum2)

#create custom plot formatting function
source("winter_report/IEP_Status&Trends_util.R")

#set up facet labels
season_names<-c('Q1'="December-February",'Q2'="March-May",'Q3'="June-August",'Q4'="September-November")
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

allplots = function(data, param){
  dat = filter(data, AnalyteName == param)
  p <- ggplot(dat, aes(x=qyear, y=value))+
    geom_line(colour="black")+geom_point(colour="black") +
    geom_hline(data = filter(wqsum2, AnalyteName == param),
               aes(yintercept = value), size = 0.9, color = "red", linetype = "dashed")+
    theme_iep() + facet_grid(quarter~region,
                             labeller = as_labeller(
                               c(region_names,season_names))) +
    theme(legend.position="none") + 
    scale_y_continuous(name = AnalyteName_labs[param],limits=c(0, max(dat$value)))+
    scale_x_continuous("Year", limits=c(1966,2018))
  return(p)
}

allplots(wqsum, "chla")
allplots(wqsum, "chlf")
allplots(wqsum, "temp")
allplots(wqsum, "secchi")


#now, for fall, make a plot for each region, season, and AnalyteName separately
#do this so you can patch them together as grobs (like fish data panel)

#first a function
WQplot = function(reg, quart, analyte, data) {
  #filter the dataset based on season, analyte, and region
  dat = filter(data, quarter == quart, region ==reg, AnalyteName == analyte)
  
  #set up limits for plot based on the max and min for all years and regions
  dat2 = filter(data, AnalyteName == analyte)
  lims = c(min(dat2$value), max(dat2$value))
    
    #make the plot
  p_sec <- ggplot(dat, aes(x=qyear, y= value))+
    geom_line(colour="black", size = 0.9)+geom_point(colour="black", size = 1.6) +
    geom_hline(aes(yintercept = mean(dat$value)), size = 0.9, color = "red", linetype = "dashed")+
    theme_iep() + 
    theme(legend.position="none") + 
    scale_y_continuous(AnalyteName_labs[analyte] , limits=lims)+
    scale_x_continuous(paste("Year(", season_names[dat[1,"quarter"]],")", sep = ""),  
                       limits=c(1966,2018)) 
  return(p_sec)
}

#winter temperature plot
tmps<-plot_grid(WQplot("spl", "Q1", "temp", wqsum),
                WQplot("ss", "Q1", "temp", wqsum), 
                WQplot("dt", "Q1", "temp", wqsum),
                ncol = 3, nrow = 1, align="v")
tmps

#save it
ggsave(tmps, file="temp_panel_winter.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./winter_report")


#spring tempearture plot
tmpsf<-plot_grid(WQplot("spl", "Q2", "temp", wqsum),
                 WQplot("ss", "Q2", "temp", wqsum),
                 WQplot("spl", "Q2", "temp", wqsum),
  ncol = 3, nrow = 1, align="v")
tmpsf
#save it
ggsave(tmpsf, file="temp_panel_spring.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./spring_report")


#summer tempearture plot
tmpss<-plot_grid(WQplot("spl", "Q3", "temp", wqsum),
                 WQplot("ss", "Q3", "temp", wqsum),
                 WQplot("spl", "Q3", "temp", wqsum),
                 ncol = 3, nrow = 1, align="v")
tmpss
#save it
ggsave(tmpss, file="temp_panel_summer.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./summer_report")


#fall tempearture plot
tmpsf<-plot_grid(WQplot("spl", "Q4", "temp", wqsum),
                 WQplot("ss", "Q4", "temp", wqsum),
                 WQplot("spl", "Q4", "temp", wqsum),
                 ncol = 3, nrow = 1, align="v")
tmpsf
#save it
ggsave(tmpsf, file="temp_panel_fall.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./fall_report")


#winter secchi plot
secsw<-plot_grid(WQplot("spl", "Q1", "secchi", wqsum),
                WQplot("ss", "Q1", "secchi", wqsum), 
                WQplot("dt", "Q1", "secchi", wqsum),
                ncol = 3, nrow = 1, align="v")

#save it
ggsave(secsw, file="secchi_panel_winter.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./winter_report")


#spring secchi plot
secsf<-plot_grid(WQplot("spl", "Q2", "secchi", wqsum),
                 WQplot("ss", "Q2", "secchi", wqsum),
                 WQplot("spl", "Q2", "secchi", wqsum),
                 ncol = 3, nrow = 1, align="v")
secsf
#save it
ggsave(secsf, file="secchi_panel_spring.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./spring_report")


#summer secchi plot
secss<-plot_grid(WQplot("spl", "Q3", "secchi", wqsum),
                 WQplot("ss", "Q3", "secchi", wqsum),
                 WQplot("spl", "Q3", "secchi", wqsum),
                 ncol = 3, nrow = 1, align="v")
secss
#save it
ggsave(secss, file="secchi_panel_summer.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./summer_report")


#fall secchie plot
secsf<-plot_grid(WQplot("spl", "Q4", "secchi", wqsum),
                 WQplot("ss", "Q4", "secchi", wqsum),
                 WQplot("spl", "Q4", "secchi", wqsum),
                 ncol = 3, nrow = 1, align="v")
secsf
#save it
ggsave(secsf, file="secchi_panel_fall.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./fall_report")


#winter chla plot
secsw<-plot_grid(WQplot("spl", "Q1", "chla", wqsum),
                 WQplot("ss", "Q1", "chla", wqsum), 
                 WQplot("dt", "Q1", "chla", wqsum),
                 ncol = 3, nrow = 1, align="v")

#save it
ggsave(secsw, file="chla_panel_winter.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./winter_report")


#spring chla plot
secsf<-plot_grid(WQplot("spl", "Q2", "chla", wqsum),
                 WQplot("ss", "Q2", "chla", wqsum),
                 WQplot("spl", "Q2", "chla", wqsum),
                 ncol = 3, nrow = 1, align="v")
secsf
#save it
ggsave(secsf, file="chla_panel_spring.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./spring_report")


#summer chla plot
secss<-plot_grid(WQplot("spl", "Q3", "chla", wqsum),
                 WQplot("ss", "Q3", "chla", wqsum),
                 WQplot("spl", "Q3", "chla", wqsum),
                 ncol = 3, nrow = 1, align="v")
secss
#save it
ggsave(secss, file="chla_panel_summer.png", 
       dpi=300, units="cm",width=27.9,height=6.8,
       path = "./summer_report")


#fall chlae plot
secsf<-plot_grid(WQplot("spl", "Q4", "chla", wqsum),
                 WQplot("ss", "Q4", "chla", wqsum),
                 WQplot("spl", "Q4", "chla", wqsum),
                 ncol = 3, nrow = 1, align="v")
secsf
#save it
ggsave(secsf, file="chla_panel_fall.png", dpi=300, units="cm",width=27.9,height=6.8,
       path = "./fall_report")



