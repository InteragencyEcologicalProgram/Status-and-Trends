#IEP Status and Trends Report
#Fall and winter Season
#Data up to and including 2017
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


#import the wq data and the station info
#old<-read.csv("WQ_Discrete_1975-1995.csv")
#new<-read.csv("WQ_Discrete_1996-2017.csv")
#alldata<-do.call("rbind", list(old,new)) #combine the two data chunks 

#EMP WQ data (1975-2017)
alldata<-read.csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/WQ_Discrete_1975-2017.csv") 
str(alldata)

#GPS coordinates of all sites
siteloc<-read.csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/wq_stations.csv") 
str(siteloc)


#format date column so R recognizes it as such
alldata$date<-as.POSIXct(alldata$date, format="%m/%d/%Y")
#looks like it figured out that I wanted the time removed

#convert value to numeric
#this didn't happen automatically because of the text for values below detection limit
alldata$value<-as.numeric(as.character(alldata$value))
#cases where value is below detection limit (ie, < R.L.) become NAs


#look at parameter list
parameters<-unique(alldata$parameter)

#reduce parameter list to just those of interest
#Total Ammonia, Field Water Temperature, Chlorophyll a, Dissolved Nitrate + Nitrite,
#Dissolved Ammonia, Field Fluorescence, Field Secchi Depth, Field Turbidity

alldat<-filter(alldata, parameter %in% c("Total Ammonia" , "Field Water Temperature" ,
                                         "Chlorophyll a", "Dissolved Nitrate + Nitrite",
                                         "Dissolved Ammonia", "Field Fluorescence" ,
                                         "Field Temperature" , "Field Secchi Depth",
                                         "Field Turbidity" ))

#now look at list of parameters again
parameters2<-unique(alldat$parameter)

#compare Total Ammonia vs Dissolved Ammonia
range(alldat$date[alldat$parameter=="Total Ammonia"]) #"1975-01-07 PST" "1978-12-07 PST"
range(alldat$date[alldat$parameter=="Dissolved Ammonia"]) #"1979-01-16 PST" "2017-12-15 PST"
#so it's the same thing but called something different through time

#compare Field Secchi Depth vs Field Turbidity
range(alldat$date[alldat$parameter=="Field Secchi Depth"]) #"1975-01-07 PST" "2017-12-15 PST"
range(alldat$date[alldat$parameter=="Field Turbidity"]) #"1975-01-07 PST" "2017-12-15 PST"
#seems like the both cover the whole date range; I wouldn't have expected that for turbidity

#compare Field Water Temperature vs Field Temperature
range(alldat$date[alldat$parameter=="Field Water Temperature"]) #"1975-01-07 PST" "1978-12-07 PST"
range(alldat$date[alldat$parameter=="Field Temperature"]) #"1979-01-16 PST" "2017-12-15 PST"

#NOTE: Field Temperature is only used as a parameter name for one observation, which isn't
#repesented in Field Water Temperature, so combine these two categories

#rename parameters with simpler names
params = data.frame(parameter = c('Field Water Temperature', "Field Temperature", "Total Ammonia", "Dissolved Ammonia",
               "Field Fluorescence","Chlorophyll a","Field Secchi Depth",
               "Field Turbidity","Dissolved Nitrate + Nitrite"),
paramsshort = c("temp", "temp", "ammonia", "ammonia", "chlf", "chla", "secchi", "turb", "nitro"))

alldat = left_join(alldat, params, by = "parameter") %>%
  mutate(parameter = paramsshort) %>%
  mutate(paramsshort = NULL)

#look at units
units<-unique(alldat$units)  #looks like there are several different types of length units (likely for secchi depth)

#look closer at units for secchi
unique(alldat$units[alldat$parameter=="secchi"]) #yep, Centimeters, Meters, and Feet; need to make these all same units
#nearly all are in cm; just exclude the three other observations for now

#remove secchi depth observations that aren't in cm
allda<-subset(alldat,units!="Feet" & units!="Meters")

#look at combination of site and id and look at how many cases there are of each combo
sites<-count(allda, name, id) 
#export this data frame so you can add 'id' to the df with the lat and long by hand 
#write.csv(sites,"O:/IEP/IEP_EstuaryIndicesProject/Report_Fall/WaterQualitySourceData/site_id.csv"
#         ,row.names = F)

#add lat and long to alldat
salldat<-join(allda,siteloc,type="left")

#look at combination of site and id and look at how many cases there are of each combo
sites2<-count(salldat,vars = c('site','lat','long'))
#there is a lot of variation in the number of observations per combo which is reasonable
#there are 3,066 observations that are not associated with any of the stations I am examining 

#remove rows in which site is NA (ie, those that weren't in the file with lat and long)
salldatc<-salldat[!is.na(salldat$site),]

#reorder columns
tot<-salldatc[c(7:9,1,4:6)]
head(tot)
str(tot)

#create a month column and make it an integer
tot$month<-format(as.Date(tot$date, format="%Y-%m-%d"),"%m")
tot$month<-as.integer(tot$month)

#create a year column and make it an integer
tot$year<-format(as.Date(tot$date, format="%Y-%m-%d"),"%Y")
tot$year<-as.integer(tot$year)
str(tot)


#add column for season
#a bit tricky because winter is Dec. of one year and then Jan./Feb. of the following year
#https://stackoverflow.com/questions/41234275/calculate-seasonal-mean-with-a-n-years-time-series-with-monthly-data?rq=1

#combine month and year
tot$ym <- as.yearmon(paste(tot$month, tot$year), "%m %Y")

#create a year-quarter column; the "+ 1/12" makes sure that December ends up as the first month of Q1 instead of Jan
tot$yq <- as.yearqtr(tot$ym + 1/12)

#date ranges based on quarters
range(tot$yq) # "1975 Q1" "2018 Q1"

str(tot)

#create columns that split the year and quarter into separate columns
#these will be used for plotting
tot$yq2<-tot$yq
tot<-tot %>% separate(yq2, c('qyear', 'quarter'), sep=" ")
tot$year<-as.integer(tot$year)

#count data points by quarter
table(tot$quarter)

#make quarter a factor
tot$quarter<-factor(tot$quarter)

#add column for geographic region based on longitude
tot$region<-ifelse(tot$long < -122.216, "spl", 
                    ifelse(tot$long > -122.216 & tot$long < -121.829, "ss",
                           ifelse(tot$long > -121.829, "dt",NA))) 

#make region a factor
tot$region<-factor(tot$region)


#generate means by region, year, quarter, and parameter
wqsum<-aggregate(value~region+qyear+quarter+parameter,data=tot,FUN=mean,na.rm=T)
wqsum$parameter<-as.factor(wqsum$parameter)
str(wqsum)

#make year an integer
wqsum$qyear = as.numeric(wqsum$qyear)

#generate means by region, quarter, and parameter
wqsum2<-aggregate(value~region+quarter+parameter,data=tot,FUN=mean,na.rm=T)
wqsum2$parameter<-as.factor(wqsum2$parameter)
str(wqsum2)



#look at all cases where N = 0 (ie, no samples for a given station-quarter-parameter combo)
#zeros<-subset(wqsum,N==0) #132 cases
#look at which station-quarter-parameter combos have zeros
#table(zeros$parameter) #ammonia = 9, chla = 120, chlf = 1, temp = 2

#look at all cases where N = 1 (ie, only one samples for a given station-quarter-parameter combo)
#ones<-subset(wqsum,N==1) #410 cases
#look at which station-quarter-parameter combos have ones
#table(ones$parameter) #ammonia = 90, chla = 101, chlf = 67, temp = 86, no2.no3

#look at all cases where N = 0 (ie, no samples for a given region-quarter-parameter combo)
#zeros2<-subset(rpsum,N==0) #no cases

#look at all cases where N = 1 (ie, only one samples for a given station-quarter-parameter combo)
#ones2<-subset(rpsum,N==1) #431 cases
#look at which station-quarter-parameter combos have ones
#table(ones2$parameter) #ammonia = 108, chla = 109, chlf = 12, temp = 92, no2.no3 = 110

#convert long to wide
#convert from long to wide format
dwid <- spread(wqsum, parameter, value)
str(dwid)

#change "Dissolved Nitrate + Nitrite" header
#names(dwid)[7]<-"nit"

#create subsets for each season
#looks like this converts qyear back to chr, so need to convert back to integer
fall<-subset(dwid,quarter=="Q4")
fall$qyear<-as.integer(fall$qyear)
winter<-subset(dwid,quarter=="Q1")
winter$qyear<-as.integer(winter$qyear)
spring<-subset(dwid,quarter=="Q2")
spring$qyear<-as.integer(spring$qyear)
summer<-subset(dwid,quarter=="Q3")
summer$qyear<-as.integer(summer$qyear)


#create custom plot formatting function
source("winter_report/IEP_Status&Trends_util.R")

#set up facet labels
season_names<-c('Q1'="December-February",'Q2'="March-May",'Q3'="June-August",'Q4'="September-November")
region_names<-c('dt'="Delta",'ss'="Suisun",'spl'="San Pablo")
parameter_labs = c("chlf" = "Chlorophyll Fluoresence (RFU)", 
                   "chla" = "Chlorophyll-a (ug/L)",
                   "temp" = "Temperature (C)",
                   "ammonia" = "Ammonium (mg/L)",
                   "nit" = "Dissolved Nitrate + Nitrite (mg/L)",
                   "secchi" = "Secchi Depth (cm)")

#set order of seasons and regions for plotting
dwid$quarter = factor(dwid$quarter, levels=c('Q1','Q2','Q3','Q4'))
dwid$region = factor(dwid$region, levels=c('spl','ss','dt'))


#change year from chr to int
dwid$qyear<-as.integer(dwid$qyear)
str(dwid)
 
#Create facet of plots for each parameter showing all combinations of region and season
wqsum3 = spread(wqsum2, parameter, value)

allplots = function(data, param){
  dat = filter(data, parameter == param)
  p <- ggplot(dat, aes(x=qyear, y=value))+
    geom_line(colour="black")+geom_point(colour="black") +
    geom_hline(data = filter(wqsum2, parameter == param),
               aes(yintercept = value), size = 0.9, color = "red", linetype = "dashed")+
    theme_iep() + facet_grid(quarter~region,
                             labeller = as_labeller(
                               c(region_names,season_names))) +
    theme(legend.position="none") + 
    scale_y_continuous(name = parameter_labs[param],limits=c(0, max(dat$value)))+
    scale_x_continuous("Year", limits=c(1966,2018))
  return(p)
}

allplots(wqsum, "chla")
allplots(wqsum, "chlf")
allplots(wqsum, "temp")
allplots(wqsum, "secchi")


#now, for fall, make a plot for each region and parameter separately
#do this so you can patch them together as grobs (like fish data panel)

#temperature
tempplot = function(reg, season) {
  dat = filter(season, region ==reg)
  p_temp <- ggplot(dat, aes(x=qyear, y=temp))+
    geom_line(colour="black", size = 0.9)+geom_point(colour="black", size = 1.6) +
    geom_hline(aes(yintercept = mean(dat$temp)), size = 0.9, color = "red", linetype = "dashed")+
    theme_iep() + 
    theme(legend.position="none") + 
    scale_y_continuous("Temperature in Celcius", limits=c(min(season$temp), max(season$temp)))+
    scale_x_continuous(paste("Year(", season_names[dat[1,"quarter"]],")", sep = ""),  
                       limits=c(1966,2018)) 
  return(p_temp)
}

Delta_winter_temp = tempplot("dt", winter)
Suisun_winter_temp = tempplot("ss", winter)

SP_winter_temp = tempplot("spl", winter)

Delta_fall_temp = tempplot("dt", fall)
Suisun_fall_temp = tempplot("ss", fall)
SP_fall_temp = tempplot("spl", fall)


tmps<-plot_grid(SP_winter_temp, Suisun_winter_temp, Delta_winter_temp,ncol = 3, nrow = 1, align="v")
tmps

tmpsf<-plot_grid(SP_fall_temp, Suisun_fall_temp, Delta_fall_temp,ncol = 3, nrow = 1, align="v")
tmpsf

ggsave(tmps, file="temp_panel_winter.png", dpi=300, units="cm",width=27.9,height=6.8)
ggsave(tmpsf, file="temp_panel_fall.png", dpi=300, units="cm",width=27.9,height=6.8)


#secchi depth: separate plots for different regions---------
secplot = function(reg, season) {
  dat = filter(season, region ==reg)
  p_sec <- ggplot(dat, aes(x=qyear, y=secchi))+
    geom_line(colour="black", size = 0.9)+geom_point(colour="black", size = 1.6) +
    geom_hline(aes(yintercept = mean(dat$secchi)), size = 0.9, color = "red", linetype = "dashed")+
    theme_iep() + 
    theme(legend.position="none") + 
    scale_y_continuous("Secchi depth in cm", limits=c(min(season$secchi), max(season$secchi)))+
    scale_x_continuous(paste("Year(", season_names[dat[1,"quarter"]],")", sep = ""),  
                       limits=c(1966,2018)) 
  return(p_sec)
}


#run the code for all the winter secchi depths
Delta_winter_sec = secplot("dt", winter)
Suisun_winter_sec = secplot("ss", winter)
SP_winter_sc = secplot("spl", winter)


#now do it for the fall secchi depths
Delta_fall_sec = secplot("dt", fall)
Suisun_fall_sec = secplot("ss", fall)
SP_fall_sec = secplot("spl", fall)

#now stich them together
secs<-plot_grid(SP_winter_sc, Suisun_winter_sec, Delta_winter_sec,ncol = 3, nrow = 1, align="v")
secs

secf<-plot_grid(SP_fall_sec, Suisun_fall_sec, Delta_fall_sec,ncol = 3, nrow = 1, align="v")
secf

#save the results
ggsave(secs, file="secchi_panel_winter.png", dpi=300, units="cm",width=27.9,height=6.8)
ggsave(secf, file="secchi_panel_fall.png",  dpi=300, units="cm",width=27.9,height=6.8)


#Chlorophyll: separate plots for different regions---------
chplot = function(reg, season) {
  dat = filter(season, region ==reg)
  p_ch <- ggplot(dat, aes(x=qyear, y=chla))+
    geom_line(colour="black",  size=0.9)+geom_point(colour="black", size = 1.6) +
    geom_hline(aes(yintercept = mean(dat$chla)), size = 0.9, color = "red", linetype = "dashed")+
    geom_vline(aes(xintercept = 1986), linetype = "dashed", color = "grey", size = 0.5) +
    annotate("text", x = 1990, y = max(season$chla)-5, label = "Clam Invasion", angle = 90)+
    theme_iep() + 
    theme(legend.position="none") + 
    scale_y_continuous("Chlorophyll-a (ug/L)", limits=c(min(season$chla), max(season$chla)))+
    scale_x_continuous(paste("Year(", season_names[dat[1,"quarter"]],")", sep = ""),  
                       limits=c(1966,2018)) 
  return(p_ch)
}



Delta_winter_ch = chplot("dt", winter)
Suisun_winter_ch = chplot("ss", winter)
SP_winter_ch = chplot("spl", winter)

Delta_fall_ch = chplot("dt", fall)
Suisun_fall_ch = chplot("ss", fall)
SP_fall_ch = chplot("spl", fall)


chs<-plot_grid(SP_winter_ch, Suisun_winter_ch, Delta_winter_ch,ncol = 3, nrow = 1, align="v")
chs

chf<-plot_grid(SP_fall_ch, Suisun_fall_ch, Delta_fall_ch,ncol = 3, nrow = 1, align="v")
chf



ggsave(chs, file="chla_panel_winter.png",  dpi=300, units="cm",width=27.9,height=10)
ggsave(chf, file="chla_panel_fall.png",  dpi=300, units="cm",width=27.9,height=10)


#Nutrients: Plots in color-----------

#plot nitrate/nitrite and ammonia on same plot
#start with just the delta region
(p_nut_d <- ggplot(fdt, aes(x=qyear))+
    geom_line(y=fdt$nit,color="#095E49", size=0.9)+
    geom_point(y=fdt$nit,color="#095E49",size=1.6) +
    geom_line(y=fdt$ammonia,color="black", size=0.9)+
    geom_point(y=fdt$ammonia,color="black",size=1.6) +
    theme_iep() +
    theme(legend.position="none")+ 
    scale_x_continuous("Year (September - November)", limits=c(1966,2018))+
    scale_y_continuous("Dissolved Nitrogen (mg / L)",limits=c(min(fall$ammonia),max(fall$nit))))


#then suisun
(p_nut_ss <- ggplot(fss, aes(x=qyear))+
    geom_line(y=fss$nit,color="#095E49", size=0.9)+
    geom_point(y=fss$nit,color="#095E49",size=1.6) +
    geom_line(y=fss$ammonia,color="black", size=0.9)+
    geom_point(y=fss$ammonia,color="black",size=1.6) +
    theme_iep() +
    theme(legend.position="none")+ 
    scale_x_continuous("Year (September - November)", limits=c(1966,2018)) +
    scale_y_continuous("Dissolved Nitrogen (mg / L)",limits=c(min(fall$ammonia),max(fall$nit))))

#then san pablo

#set colors
cols <- c("Ammonium"="black","Nitrate/Nitrite"="#095E49")

(p_nut_sp <- ggplot(fspl, aes(x=qyear))+
    geom_line(aes(y=fspl$nit,color="Nitrate/Nitrite"), size=0.9)+
    geom_point(aes(y=fspl$nit,color="Nitrate/Nitrite"),size=1.6)+ 
    geom_line(aes(y=fspl$ammonia,color="Ammonium"), size=0.9)+
    geom_point(aes(y=fspl$ammonia,color="Ammonium"),size=1.6) +
    theme_iep()+
    theme(legend.position=c(0.3,0.75))+ 
    scale_x_continuous("Year (September - November)", limits=c(1966,2018)) +
    scale_y_continuous("Dissolved Nitrogen (mg / L)",limits=c(min(fall$ammonia),max(fall$nit)))+
    scale_colour_manual(name="Nitrogen Form",values =cols,labels=c("Ammonium","Nitrate / Nitrite"))
)

#now patch together all three temperature plots and nutrients plots-----------
#remove x axis label from nutrient plots
#add legend to leftmost nutrient plot
#make sure everything is lining up correctly

#creates a grid of plots where everything lines up properly
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

ptn<-plot_grid(p_sec_sp,p_sec_ss,p_sec_d,p_nut_sp,p_nut_ss,p_nut_d, ncol = 3, nrow = 2, align="v")
ptn
#slow to render

#save plots as .png
#ggsave(ptn, file="wq_panel_color.png", path=plot_folder,scale=1.8,  dpi=300, units="cm",width=25.5,height=10.8)



#save plots as .png
#ggsave(pch, file="chla_fall_panel.png", path=plot_folder,scale=1.8, dpi=300, units="cm",width=25.5,height=5.4)


## Final figure:
water_quality_main_layout <- rbind(c(1,2,3),
																	 c(4,5,6))
water_quality_main_fig <- grid.arrange(
						 p_sec_sp, p_sec_ss, p_sec_d, 
						 p_nut_sp, p_nut_ss, p_nut_d, 
						 layout_matrix = water_quality_main_layout,
						 heights=unit(c(68,68), c("mm")),
						 widths=unit(c(102,102,102), c("mm")))

