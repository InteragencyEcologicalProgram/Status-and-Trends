#Seasonal Monitoring Report
#Summer 2018

#Purpose: Create plot of invasive aquatic vegetation coverage through time
#Data represent North + Central Delta only
#Coverage estimates based on hyperspectral imagery
#Data for submerged and floating aquatic vegetation types
#Data acquired from Shruti Khanna (CDFW)

#Author: Nick Rasmussen 

#required packages
library(tidyverse) #plotting data; converting between long and wide data frames
library(scales) #modifying y axis to include '%'

#install and load most up to date version of 'smonitr' from GitHub
#remotes::install_github("InteragencyEcologicalProgram/smonitr")
library(smonitr) #standardizes formatting of plots


# 1. Import Data ----------------------------------------------------------


# Dataset is on SharePoint site for the Seasonal Monitoring Report

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/IEP Synthesis Projects - Data"
  )
)  

# Import vegetation data
veg<-read.csv(file = paste0(sharepoint_path, "/AquaticVegetation_North+CentralDelta.csv"))

#examine data frame
str(veg) #looks fine


#2. Data frame manipulations in preparation for plotting-------------------


#create subset with percent coverage of waterways
plist<-c("year","sav_perc","wp_perc",   "wh_perc",   "fav_perc" )
perc<-veg[,plist]

#convert data frame from wide to long
percl<-gather(perc,type,perc,sav_perc:fav_perc,factor_key=T)

#create subset that is just total SAV & FAV
#excludes WH and WP data
ppercl<-subset(percl,type=="sav_perc" | type=="fav_perc")

#year needs to be a factor in order for some of 'smonitr' plotting functions to work
ppercl$year<-as.factor(ppercl$year)

#create subset with acres 
#alist<-c("year","sav_acres", "wp_acres",  "wh_acres",  "fav_acres")
#acr<-veg[,alist]

#convert data frame from wide to long
#acrl<-gather(acr,type,acres,sav_acres:fav_acres,factor_key=T)

#create subset that is just total SAV & FAV
#excludes WH and WP data
#sacrl<-subset(acrl,type=="sav_acres" | type=="fav_acres")

#create subset that is just SAV, WH, and WP
#excludes summarized FAV column
#spp<-subset(acrl,type!="fav_acres")


#3. Set up options for plots------------------------------

#calcuate mean for total veg % coverage (SAV + FAV) across all years
#then plot this as a horizontal red dashed line

#first sum FAV and SAV within each year
vtot<-setNames(aggregate(x=ppercl$perc,by=list(ppercl$year),FUN=sum), c("year","perc"))
str(vtot)
#calculate mean
mean(vtot$perc) #29.6

#stack bar colors
repcols<-c("sav_perc" = "#556B2F",
           "fav_perc" = "#88BA33")

#reorder factor levels for vegetation type so FAV is on top
ppercl$type = factor(ppercl$type, levels=c('fav_perc','sav_perc'))


#4. Create plots---------------------------------------------


#stacked bar plot of percent coverage
(pperc<-ggplot(data=ppercl,aes(x=year, y=perc,fill=type))
   #specifies the independent and dependent variables as well as groupings
    +geom_bar(position = "stack", stat = "identity",colour="grey25")
 #specifies that this is a stacked bar plot
  +theme_smr()
 #implements standardized plot formatting
 +ylab("Water area occupied")  
 #y-axis label
 +scale_y_continuous(labels= function(x) paste0(x, "%"))
 #adds a percent sign after each y axis tick label number
 +scale_fill_manual(name = "",labels=c("Floating","Submerged"),values=repcols, guide = guide_legend(keyheight = 0.5))
 #customizes names in legend key, specifies the custom color palette, and sets height of elements in legend
 +lt_avg_line(mean(vtot$perc))
 #adds horizontal line to plot to indicate long term average for data
 +std_x_axis_rec_years(2018, x_scale_type= "discrete")
 #implements standardized x-axis range of years
 +missing_data_symb(ppercl,year,2018, symb_size = 2)
 #adds symbols anywhere there is missing data
  +std_x_axis_label("annual")
 #adds "Year" as x-axis label
 +theme(
   legend.box.spacing = unit(0, units = "cm")
   ,plot.margin = margin(t=0,r=0,b=0,l=0)
   )
 #reduces white space between legend and top of plot
  )


#print plot

#path to location to put plot
plot_path<-"C:/Repositories/Status-and-Trends/summer_report/figures"

ggsave(
  plot=pperc,
  filename = "veg_perc.png", 
  dpi=300,
  units="cm",
  width=9.3,
  #height=6.8,
  height=7.2,
  path = plot_path 
  )
#standard plot height is 6.8 cm; Rosie is using 7.5 cm in her plots I think to accommodate the legend


#stacked bar plot of acres: FAV and SAV

#(pacr<-ggplot(sacrl,aes(x=year, y=acres))+
#  geom_col(aes(fill=type))+theme_iep()
#  )





