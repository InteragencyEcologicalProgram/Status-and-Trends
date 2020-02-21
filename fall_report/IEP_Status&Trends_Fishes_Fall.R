#IEP Status and Trends Report
#Fall Season
#Data up to and including 2017
#Fish plots panel (six graphs)
#FMWT: Delta smelt, longfin smelt, striped bass, American shad
#also white sturgeon, fall run chinook salmon

#Created by Nick Rasmussen
#udated by Rosemary Hartman 2/18/2020

#packages
library(ggplot2) 
library(cowplot) #plot_grid()
library(tidyverse)

#import data

#fall midwater trawl: Delta smelt, longfin smelt, striped bass, American shad
fmwt<-read.csv("data/fmwt.csv")
str(fmwt)

#CA Fish & Wildlife: Sturgeon Trammel Net Surveys
wst<-read.csv("data/wst.csv")
str(wst)

#Fall run chinook salmon counts
frch<-read.csv("data/Grandtab_adultsalmon.csv")
str(frch)

#create custom plot formatting function
theme_iep <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          axis.title.x = element_text(size = 10, face = "plain"),
          axis.title.y = element_text(size = 10, face = "plain"
                                      ,margin=margin(t = 0, r = 10, b = 0, l = 0)
          ),             
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          #plot.margin = unit(c(0.1, 0.3, 0.1, 0.9), units = , "cm"), #top, right, bottom, left
          plot.margin = unit(c(0.25, 0.4, 0.1, 0.4), units = , "cm"), #adjusted the "top" and "right" values so nothing is cut off
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 9, face = "plain"),
          legend.title=element_text(size=10))
}


### Plot Fall Midwater Trawl species -----------------------------------

#create subsets
ds<-subset(fmwt,species=="ds")
lfs<-subset(fmwt,species=="lfs")
sb0<-subset(fmwt,species=="sb0")
as<-subset(fmwt,species=="as")
 

#Delta Smelt

#Traditional Index 1967-2017
(p_ds <- ggplot(ds, aes(x=year, y=index/1000))+
  geom_bar(stat="identity",fill="#095E49") +
  theme_iep() +
  theme(legend.position="none") + 
  scale_y_continuous("Delta Smelt Index (? 1,000)",limits=c(0, max(ds$index)/1000)) +
  scale_x_continuous("", limits=c(1966,2018))) 
#ggsave(p_ds, file="ds_trad_fmwt_color.png", path=plot_folder,scale=2,
#       dpi=300, units="cm",width=8.5,height=5.4)
#NOTE: need to specify path for plot_folder before saving


#Longfin Smelt

#Traditional Index 1967-2017
(p_lfs <- ggplot(lfs, aes(x=year, y=index/1000))+
    geom_bar(stat="identity",fill="#095E49") +
    theme_iep() +
    theme(legend.position="none") + 
    scale_y_continuous("Longfin Smelt Index (? 1,000)",limits=c(0, max(lfs$index)/1000)) +
    scale_x_continuous("Year", limits=c(1966,2018)) )
#ggsave(p_lfs, file="lfs_trad_fmwt_color.png", path=plot_folder,scale=2,
#       dpi=300, units="cm",width=8.5,height=5.4)
#NOTE: need to specify path for plot_folder before saving


#Striped Bass

#Traditional Index 1967-2017
(p_sb0 <- ggplot(sb0, aes(x=year, y=index/1000))+
  geom_bar(stat="identity",fill="#095E49") +
  theme_iep() +
  theme(legend.position="none") + 
  scale_y_continuous("Striped Bass Index (? 1,000)",limits=c(0, max(sb0$index)/1000)) +
  scale_x_continuous("", limits=c(1966,2018)) )
#ggsave(p_sb0, file="sb0_trad_fmwt_color.png", path=plot_folder,scale=2,
#       dpi=300, units="cm",width=8.5,height=5.4)
#NOTE: need to specify path for plot_folder before saving


#American Shad

#Traditional Index 1967-2017
(p_as <- ggplot(as, aes(x=year, y=index/1000))+
   geom_bar(stat="identity",fill="#095E49") +
   theme_iep() +
   theme(legend.position="none") + 
   scale_y_continuous("American Shad Index (? 1,000)",limits=c(0, max(as$index)/1000)) +
   scale_x_continuous("Year", limits=c(1966,2018)) )
#ggsave(p_as, file="as_trad_fmwt_color.png", path=plot_folder,scale=2,
#       dpi=300, units="cm",width=8.5,height=5.4)
#NOTE: need to specify path for plot_folder before saving



### Plot sturgeon and salmon -------------------------------


#White Sturgeon
#unlike FMWT, these data actually start 1968 instead of 1966
(p_wst <- ggplot(wst, aes(x=Year, y=AvgCPUE))+
  geom_bar(stat="identity",fill="#664F2B") +
  theme_iep() +
  theme(legend.position="none") + 
  scale_y_continuous("White Sturgeon CPUE",limits=c(0, max(wst$AvgCPUE))) +
  scale_x_continuous("", limits=c(1966,2018)) )


#fall run chinook salmon

(p_frch <- ggplot(frch, aes(x=Year, y=FallRun/100000))+
    geom_bar(stat="identity", fill="black") +
    theme_iep() +
    theme(legend.position="none") + 
    scale_y_continuous("Fall Run Chinook Counts (? 100,000)",limits=c(0, max(frch$FallRun)/100000)) +
    scale_x_continuous("Year", limits=c(1966,2018)) )


#creates a grid of plots where everything lines up properly--------------
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

fishf<-plot_grid(p_ds,p_sb0,p_wst,p_lfs,p_as,p_frch, ncol = 3, nrow = 2, align="v")
fishf #can be slow to render

#save plot panel as .png
#ggsave(fishf, file="fish_panel_trad_fmwt_color.png", path=plot_folder,scale=1.8, dpi=300, units="cm",width=25.5,height=10.8)
#NOTE: need to set path for plot_folder before saving

### All six fish plots showing just recent trends (2002-2017)--------------------

#create subsets of data

dsa<-subset(ds,year>2001 ) #Delta Smelt
lsa<-subset(lfs,year>2001 ) #Longfin Smetl
sba<-subset(sb0,year>2001 ) #Striped Bass
asa<-subset(as,year>2001 ) #American Shad
frcha<-subset(frch,Year>2001 ) #chinook salmon

#White sturgeon
#add two years that have missing values as zeros to help with plotting error
#seems like this did fix plotting issue
wstaa<-subset(wst,Year>2001 ) 
wstaq<-wstaa[c(1,5)]
t<-data.frame(cbind(c(2003,2004),c(0,0)))
names(t)<-c("Year","AvgCPUE")
wsta<-rbind(wstaq,t)
wsta$Year<-as.integer(wsta$Year)
wsta$AvgCPUE<-as.numeric(wsta$AvgCPUE)
str(wsta)

#Delta Smelt

#Traditional Index 2002-2017
(p_dsv <- ggplot(dsa, aes(x=year, y=index/1000))+
   geom_bar(stat="identity",fill="#095E49") +
   theme_iep() +
   theme(legend.position="none") + 
   scale_y_continuous("Delta Smelt Index (? 1,000)",limits=c(0, max(dsa$index)/1000)) +
   scale_x_continuous("", limits=c(2001,2018))) 
#ggsave(p_dsv, file="ds_trad_fmwt_recent_color.png", path=plot_folder,scale=2,
#      dpi=300, units="cm",width=8.5,height=5.4)

#Longfin Smelt

#Traditional Index 2002-2017
(p_lfsv <- ggplot(lsa, aes(x=year, y=index/1000))+
    geom_bar(stat="identity",fill="#095E49") +
    theme_iep() +
    theme(legend.position="none") + 
    scale_y_continuous("Longfin Smelt Index (? 1,000)",limits=c(0, max(lsa$index)/1000)) +
    scale_x_continuous("Year", limits=c(2001,2018)) )

#Striped Bass

#Traditional Index 2002-2017
(p_sb0v <- ggplot(sba, aes(x=year, y=index/1000))+
    geom_bar(stat="identity",fill="#095E49") +
    theme_iep() +
    theme(legend.position="none") + 
    scale_y_continuous("Striped Bass Index (? 1,000)",limits=c(0, max(sba$index)/1000)) +
    scale_x_continuous("", limits=c(2001,2018)) )

#American Shad

#Traditional Index 2002-2017
(p_asv <- ggplot(asa, aes(x=year, y=index/1000))+
    geom_bar(stat="identity",fill="#095E49") +
    theme_iep() +
    theme(legend.position="none") + 
    scale_y_continuous("American Shad Index (? 1,000)",limits=c(0, max(asa$index)/1000)) +
    scale_x_continuous("Year", limits=c(2001,2018)) )

#White Sturgeon
(p_wstv <- ggplot(wsta, aes(x=Year, y=AvgCPUE))+
    geom_bar(stat="identity",fill="#664F2B") +
    theme_iep() +
    theme(legend.position="none") + 
    scale_y_continuous("White Sturgeon CPUE",limits=c(0, max(wsta$AvgCPUE))) +
    scale_x_continuous("", limits=c(2001,2018)) )

#Chinook Salmon
(p_frchv <- ggplot(frcha, aes(x=Year, y=FallRun/100000))+
    geom_bar(stat="identity", fill="black") +
    theme_iep() +
    theme(legend.position="none") + 
    scale_y_continuous("Fall Run Chinook Counts (? 100,000)",limits=c(0, max(frcha$FallRun)/100000)) +
    scale_x_continuous("Year", limits=c(2001,2018)) )


#creates a grid of plots where everything lines up properly
fishs<-plot_grid(p_dsv,p_sb0v,p_wstv,p_lfsv,p_asv,p_frchv, ncol = 3, nrow = 2, align="v")
fishs #very slow to render

#save plots as .png
#ggsave(fishs, file="fish_panel_subset.png", path=plot_folder,scale=1.8,dpi=300, units="cm",width=25.5,height=10.8)
#NOTE: need to set path for plot_folder before saving



## Final figures:
fish1_main_layout <- rbind(c(1,2,3),
													 c(4,5,6))
fish1_main_fig <- grid.arrange(
						 p_ds, p_sb0, p_wst, 
						 p_lfs, p_as, p_frch, 
						 layout_matrix = fish1_main_layout,
						 heights=unit(c(68,68), c("mm")),
						 widths=unit(c(102,102,102), c("mm")))


fish2_main_layout <- rbind(c(1,2,3),
													 c(4,5,6))
fish2_main_fig <- grid.arrange(
						 p_dsv, p_sb0v, p_wstv, 
						 p_lfsv, p_asv, p_frchv, 
						 layout_matrix = fish2_main_layout,
						 heights=unit(c(68,68), c("mm")),
						 widths=unit(c(102,102,102), c("mm")))

