#Seasonal Monitoring Report
#Summer 2018
#Invasive aquatic vegetation: North + Central Delta only
#Coveraged based on hyperspectral imagery
#Data acquired from Shruti Khanna (CDFW)

#required packages
library(tidyverse) #plotting data; converting between long and wide data frames
library(colorspace) #plot color palettes
library(scales) #modifying y axis to include '%'
library(devtools) #needed for installing 'smonitr' from GitHub

#install most up to date version of 'smonitr' from GitHub
devtools::install_github("InteragencyEcologicalProgram/Status-and-Trends/smonitr")
library(smonitr) 


#import data
veg<-read.csv("veg.csv")
#sharepoint location
#https://cawater.sharepoint.com/:x:/r/sites/dwr-str/Shared%20Documents/Data/veg.csv?d=wdb4811ae33cf4e5ca21ae1fd36ebf5cd&csf=1&e=Ee6pdO
str(veg)

#working directory
setwd("C:/Users/nrasmuss/OneDrive - California Department of Water Resources/IEP/IEP_EstuaryIndicesProject/Report_Summer")
#setwd("C:/Repositories/Status-and-Trends/summer_report")



#create subset with acres 
#alist<-c("year","sav_acres", "wp_acres",  "wh_acres",  "fav_acres")
#acr<-veg[,alist]

#convert from wide to long
#acrl<-gather(acr,type,acres,sav_acres:fav_acres,factor_key=T)

#create subset that is just total SAV & FAV
#excludes WH and WP data
#sacrl<-subset(acrl,type=="sav_acres" | type=="fav_acres")

#create subset that is just total SAV, WH, and WP
#excludes summarized FAV column
#spp<-subset(acrl,type!="fav_acres")

#create subset with percent coverage of waterways
plist<-c("year","sav_perc","wp_perc",   "wh_perc",   "fav_perc" )
perc<-veg[,plist]

#convert from wide to long
percl<-gather(perc,type,perc,sav_perc:fav_perc,factor_key=T)

#create subset that is just total SAV & FAV
#excludes WH and WP data
ppercl<-subset(percl,type=="sav_perc" | type=="fav_perc")

#stacked bar plot of acres: FAV and SAV

#(pacr<-ggplot(sacrl,aes(x=year, y=acres))+
#  geom_col(aes(fill=type))+theme_iep()
#  )
#ggsave("veg_acres.png",scale=0.7, height=8,width=11, units="in",dpi=300)


#stacked bar plot of percent coverage

#create df that will indicate where the missing data point labels will be positioned on the graph

#first create df with all possible years and veg type levels
#yrs<-seq(2004,2018,1)
#vt<-c("sav_perc","fav_perc")
#aveg<-data.frame(expand.grid(yrs,vt))
#names(aveg)<-c("year","type")
#str(aveg)

#use anti-join to combine the complete 'aveg' df with 'ppercl' df which excludes missing data
#this will create a new df with just the combinations with missing data
#md<-anti_join(aveg,ppercl) #throws a warning but df looks fine
#str(md)

#add a new column to 'md' df that will provides the number representing the position of the '*' on the y axis
#should be low (e.g., 0, 1, or 2)
#md$y<-"0"
#md$y<-as.numeric(md$y)
#str(md)

#calcuate mean for total veg % coverage (SAV + FAV) across all years
#then plot this as a horizontal red dashed line

#first add FAV and SAV within each year
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


(pperc<-ggplot(ppercl,aes(x=year, y=perc,fill=type))+
    geom_bar(position = "stack", stat = "identity",colour="grey25")+
    ylab("Water area occupied") + xlab("Year")+
    scale_y_continuous(labels= function(x) paste0(x, "%"))+
    scale_fill_manual(name = "",labels=c("Floating","Submerged")
                      ,values=repcols)+
    theme(legend.position=c(0.38,0.85),legend.direction="horizontal")+
    missing_data_symb(ppercl,year,2018)+
    theme_smr()+
    lt_avg_line(mean(vtot$perc))+
    std_x_axis_rec_years(2018)
)
#ggsave("veg_perc.png", height=6.8,width=9.3, units="cm",dpi=300)







