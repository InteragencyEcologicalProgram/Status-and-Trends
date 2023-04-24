#Seasonal Monitoring Report
#Summer 2020

#Purpose: Create plot of invasive aquatic vegetation coverage through time
#Data represent North + Central Delta only
#Coverage estimates based on hyperspectral imagery
#Data for submerged and floating aquatic vegetation types
#Data acquired from Shruti Khanna (CDFW)

#Author: Nick Rasmussen 
#Updated: April 18, 2023

source("setup.R")

#required packages
library(tidyverse) #suite of data science tools
library(scales) #modifying y axis to include '%'

#install and load most up to date version of 'smonitr' from GitHub
#remotes::install_github("InteragencyEcologicalProgram/smonitr")
library(smonitr) #standardizes formatting of plots


# 1. Import Data ----------------------------------------------------------

# Import vegetation data
veg<-read_csv(file = "data/AquaticVegCoverage_CSTARS_report.csv") %>% 
  glimpse()

#2. Data frame manipulations in preparation for plotting-------------------

veg_ft <- veg %>% 
  #just keep the needed columns
  select(year, sav_prop,wh_prop,wp_prop) %>% 
  rename(sav = sav_prop) %>% 
  #the original fav_tot_prop column includes pennywort
  #lets make a new one that is just water hyacinth and water primrose
  mutate(fav = wh_prop + wp_prop) %>% 
  #drop unneeded columns
  select(-c(wh_prop,wp_prop)) %>% 
  #convert to long format
  pivot_longer(cols = sav:fav, names_to = "type", values_to = "prop") %>% 
  #convert proportions to percentages
  mutate(perc = prop*100) %>% 
  #drop the proportion column
  select(-prop)

#3. Set up options for plot------------------------------

#calculate mean for total veg % coverage (SAV + FAV) across all years
#then plot this as a horizontal red dashed line

veg_sum <- veg_ft %>%
  #sum FAV and SAV within each year
  group_by(year) %>% 
  summarize(tot_perc = sum(perc)) %>% 
  #then calculate annual mean for total veg
  mutate(tot_perc_mean = mean(tot_perc,na.rm=T)
         ,tot_perc_diff = tot_perc - tot_perc_mean
         )
#mean is 22.14167%
#In 2020, the percentage of water area occupied by aquatic vegetation in the Delta
#was 7.3% higher than the long-term mean

#reorder factor levels for vegetation type so FAV is on top
veg_ft$type = factor(veg_ft$type, levels=c('fav','sav'))


#4. Create and export plot---------------------------------------------

#stacked bar plot of percent coverage
(veg_perc <- ggplot(data=veg_ft,aes(x=year, y=perc, fill=type)) + 
	#specifies the independent and dependent variables as well as groupings
	geom_bar(position="stack", stat="identity", colour="grey25")+  
	#specifies that this is a stacked bar plot
	smr_theme_update()+
	#implements standardized plot formatting
	ylab("Water area occupied") + xlab("Year")+
	#x- and y-axis labels
	scale_y_continuous(labels=function(x) paste0(x, "%"))+  # use default expansion
	#adds a percent sign after each y axis tick label number
	scale_fill_manual(name= NULL
	                  ,labels=c("Floating","Submerged")
	                  ,values=c("#88BA33","#556B2F")
	                  ,guide=guide_legend(keyheight=0.5)
	                  )  +
	#customizes names in legend key, specifies the custom color palette, and sets height of elements in legend
	stat_lt_avg(data=veg_sum, aes(y=tot_perc_mean), inherit.aes=FALSE)  +
	#adds horizontal line to plot to indicate long term mean for data
	smr_x_axis(report_year, type="recent", season="annual")  +
	#implements standardized x-axis range of years
  stat_missing(size=2.5) + 
  #stat_missing(size=2, nudge_y=max(veg_ft$perc)*0.02) + 
  #adds symbols for missing data, customizes symbol size, and nudged the symbol a little above x-axis (2% of y-axis range)
	theme(#legend.key.size=unit(0.3,"cm"), 
				#legend.spacing.x=unit(0.1, 'cm'),  
				legend.box.spacing=unit(0, units="cm"), 
				legend.margin=margin(t=0,r=0,b=2,l=0, unit="pt")) + 
	#reduces white space between legend and top of plot
	smr_caption(data=veg_sum, aes(x=year, y=tot_perc), inherit.aes=FALSE, 
							stat_name="percentage of water area occupied by aquatic vegetation", 
							report_year=report_year)+  
	smr_alttext(data=veg_sum, aes(x=year, y=tot_perc), inherit.aes=FALSE, 
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

