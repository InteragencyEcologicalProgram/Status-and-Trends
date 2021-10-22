#Seasonal Monitoring Report
#Summer 2019

#Purpose: Create plot of invasive aquatic vegetation coverage through time
#Data represent North + Central Delta only
#Coverage estimates based on hyperspectral imagery
#Data for submerged and floating aquatic vegetation types
#Data acquired from Shruti Khanna (CDFW)

#Author: Nick Rasmussen 
#Updated: October 2, 2020

#required packages
library(scales) #modifying y axis to include '%'

#install and load most up to date version of 'smonitr' from GitHub
#remotes::install_github("InteragencyEcologicalProgram/smonitr")
#library(smonitr) #standardizes formatting of plots


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
#veg<-read.csv(file = paste0(sharepoint_path, "/AquaticVegetation_North+CentralDelta.csv"))
veg<-read.csv(file = paste0(data_root, "/AquaticVegetation_North+CentralDelta.csv"))

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


#3. Set up options for plot------------------------------

#calcuate mean for total veg % coverage (SAV + FAV) across all years
#then plot this as a horizontal red dashed line

#sum FAV and SAV within each year
vtot<-setNames(aggregate(x=ppercl$perc,by=list(ppercl$year),FUN=sum), c("year","perc"))
#str(vtot)

#calculate mean
mean(vtot$perc) #29.975

#calculate percent change between long-term mean and most recent year
(vtot$perc[length(vtot$perc)]-mean(vtot$perc))/mean(vtot$perc)*100 #8.757298
#In 2019, the percentage of water area occupied by aquatic vegetation in the Delta was 9% higher than the long-term mean

(abs(vtot$perc[length(vtot$perc)]-mean(vtot$perc)))/sd(vtot$perc) #0.2372518
#2019 value is 0.24 SD from mean

#stack bar colors
repcols<-c("sav_perc" = "#556B2F",
           "fav_perc" = "#88BA33")

#reorder factor levels for vegetation type so FAV is on top
ppercl$type = factor(ppercl$type, levels=c('fav_perc','sav_perc'))


#4. Create and export plot---------------------------------------------

#stacked bar plot of percent coverage
(veg_perc <- ggplot(data=ppercl,aes(x=year, y=perc, fill=type)) + 
	#specifies the independent and dependent variables as well as groupings
	geom_bar(position="stack", stat="identity", colour="grey25") + 
	#specifies that this is a stacked bar plot
	smr_theme_update() + 
	#implements standardized plot formatting
	ylab("Water area occupied") + 
	#y-axis label
	scale_y_continuous(labels=function(x) paste0(x, "%")) + # use default expansion
	# # # smr_y_axis(labels=function(x) paste0(x, "%")) + 
	#adds a percent sign after each y axis tick label number
	scale_fill_manual(name= "", labels=c("Floating","Submerged"),
										values=repcols, guide=guide_legend(keyheight=0.5)) + 
	#customizes names in legend key, specifies the custom color palette, and sets height of elements in legend
	stat_lt_avg(data=vtot, aes(y=perc), inherit.aes=FALSE) + 
	#adds horizontal line to plot to indicate long term average for data
	smr_x_axis(report_year, type="recent", season="annual") + 
	#implements standardized x-axis range of years
	# # # stat_missing(size=2, nudge_y=max(vtot$perc)*0.02) + 
	#adds symbols for missing data, customizes symbol size, and nudged the symbol a little above x-axis (2% of y-axis range)
	theme(#legend.key.size=unit(0.3,"cm"), 
				#legend.spacing.x=unit(0.1, 'cm'),  
				legend.box.spacing=unit(0, units="cm"), 
				legend.margin=margin(t=0,r=0,b=2,l=0, unit="pt")) + 
	#reduces white space between legend and top of plot
	smr_caption(data=vtot, aes(x=year, y=perc, fill=NULL), 
							stat_name="percentage of water area occupied by aquatic vegetation", 
							report_year=report_year) + 
	smr_alttext(data=vtot, aes(x=year, y=perc, fill=NULL), 
							stat_name="percentage of water area occupied by aquatic vegetation")
)

getCaption(veg_perc)
getAlttext(veg_perc)


save(list="veg_perc", file=file.path(fig_root_summer,"veg_perc.RData"))


#print plot

#path to location to put plot
# plot_path<-"C:/Repositories/Status-and-Trends/summer_report/figures"

# ggsave(
  # plot=pperc,
  # filename = "veg_perc.png", 
  # dpi=300,
  # units="cm",
  # width=9.3,
  # #height=6.8,
  # height=7.2,
  # path = fig_root_summer 
  # )
# #standard plot height is 6.8 cm; Sam is using 7.5 cm in her plots I think to accommodate the legend

