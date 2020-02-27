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
library(devtools) #needed for installing 'smonitr' from GitHub

#install and load most up to date version of 'smonitr' from GitHub
#devtools::install_github("InteragencyEcologicalProgram/Status-and-Trends/smonitr")
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
mean(vtot$perc) #29.6

#stack bar colors
repcols<-c("sav_perc" = "#556B2F",
           "fav_perc" = "#A2CD5A")

#reorder factor levels for vegetation type so FAV is on top
ppercl$type = factor(ppercl$type, levels=c('fav_perc','sav_perc'))


#function for missing data symbol
missing_data_symb <- function(df, yr_var, rpt_yr) {
  # Convert yr_var to enquo for non-standard eval
  yr_var_enquo <- rlang::enquo(yr_var)
  
  # Calculate minimum year in df
  yr_min <- min(dplyr::pull(df, !!yr_var_enquo))
  
  # Create a tibble with all possible years
  all_yrs <- tibble::tibble(
    years = seq(yr_min, rpt_yr, 1),
    result = 0
  )
  
  # Find missing years in df
  missing_yrs <- dplyr::anti_join(
    all_yrs,
    df,
    by = c("years" = rlang::as_name(yr_var_enquo))
  )
  
  # Add in symbols for missing years if necessary
  if (!is.null(missing_yrs)) {
    geom_point(
      data = missing_yrs,
      aes(
        x = years,
        y = result
      ),
      inherit.aes = FALSE,
      na.rm = TRUE,
      shape = 8,
      size = 1,
      color = "red"
    )
  }
  
}


#4. Create plots---------------------------------------------


#stacked bar plot of percent coverage
pperc<-ggplot(ppercl,aes(x=year, y=perc,fill=type))+
   #specifies the dependent and independent variables 
    geom_bar(position = "stack", stat = "identity",colour="grey25")+
   #specifies that this is a stacked bar plot
    ylab("Water area occupied") + xlab("Year")+
   #axis labels
    scale_y_continuous(labels= function(x) paste0(x, "%"))+
   #adds a percent sign after each y axis tick label numbers
    scale_fill_manual(name = "",labels=c("Floating","Submerged"),values=repcols)+
   #customizes names in legend key and specifies the custom color palette
  theme_smr()+
  #implements standardized plot formatting
    theme(legend.position=c(0.38,0.85),legend.direction="horizontal")+
   #specifies location of legend and that elements of legend should be arranged horizontally
    missing_data_symb(ppercl,year,2018)+
   #adds symbols anywhere there is missing data
    lt_avg_line(mean(vtot$perc))+
   #adds horizontal line to plot to indicate long term average for data
    std_x_axis_rec_years(2018)
   #standardizes the x axis range


#print plot

#path to location to put plot
plot_path<-"C:/Repositories/Status-and-Trends/summer_report"

ggsave(
  plot=pperc,
  filename = "veg_perc.png", 
  dpi=300,
  units="cm",
  width=9.3,
  height=6.8,
  path = plot_path 
  )


#stacked bar plot of acres: FAV and SAV

#(pacr<-ggplot(sacrl,aes(x=year, y=acres))+
#  geom_col(aes(fill=type))+theme_iep()
#  )
#ggsave("veg_acres.png",scale=0.7, height=8,width=11, units="in",dpi=300)





