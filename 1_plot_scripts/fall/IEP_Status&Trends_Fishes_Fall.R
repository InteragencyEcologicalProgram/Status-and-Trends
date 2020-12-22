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
library(smonitr)
library(readr)

#import data

#fall midwater trawl: Delta smelt, longfin smelt, striped bass, American shad
fmwt<-read_csv(file.path(data_root,"fmwt.csv"))
str(fmwt)

#go from long to wide
fmwt = pivot_longer(fmwt, cols = `Threadfin Shad`:`Striped Bass Age0`, 
                    names_to = "Species", values_to = "Index")

#remove years with "NAs" so that the missing data symbol works, 
fmwt = filter(fmwt, !is.na(Index)) 

#CA Fish & Wildlife: Sturgeon Trammel Net Surveys
wst<-read.csv(file.path(data_root,"wst.csv"))
str(wst)
wst = mutate(wst, fyear = as.factor(Year))

### Plot Fall Midwater Trawl species -----------------------------------

#create subsets
ds<-subset(fmwt,Species=="Delta Smelt")
lfs<-subset(fmwt,Species=="Longfin Smelt")
sb0<-subset(fmwt,Species=="Striped Bass Age0")
as<-subset(fmwt,Species=="American Shad")
 

#Delta Smelt

#Traditional Index 1967-2019
p_ds <- ggplot(ds, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
  smr_theme() +
theme(legend.position="none") + 
  scale_y_continuous("Delta Smelt Index") +
  smr_x_axis(report_year, type = "all", season = "fall") +
 stat_missing()+
  stat_lt_avg()
p_ds
ggsave(p_ds, file="FMWT_DS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)
# ggsave(p_ds, file="FMWT_DS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")


#Longfin Smelt

#Traditional Index 1967-2019
p_lfs <- ggplot(lfs, aes(x=Year, y=Index))+
    geom_bar(stat="identity") +
    smr_theme() +
  smr_x_axis(report_year, type = "all", season = "fall") +
    theme(legend.position="none") + 
    scale_y_continuous("Longfin Smelt Index") +
  stat_missing()+
  stat_lt_avg()
p_lfs  
  
ggsave(p_lfs, file="FMWT_LFS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)


#Striped Bass

#Traditional Index 1967-2019
p_sb0 <- ggplot(sb0, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
  smr_theme() +
  theme(legend.position="none") + 
  scale_y_continuous("Striped Bass Index") +
  smr_x_axis(report_year, type = "all", season = "fall") +
  stat_missing()+
  stat_lt_avg()

ggsave(p_sb0, file="FMWT_SB0_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)

#American Shad

#Traditional Index 1967-2017
p_as <- ggplot(as, aes(x=Year, y=Index))+
   geom_bar(stat="identity") +
  smr_theme() +
   theme(legend.position="none") + 
    scale_y_continuous("American Shad Index")+
    smr_x_axis(report_year, type = "all", season = "fall") +
    stat_missing()+
    stat_lt_avg()

p_as
  
ggsave(p_as, file="FMWT_AS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)
# ggsave(p_as, file="FMWT_AS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")




### Plot sturgeon  -------------------------------


#White Sturgeon
#unlike FMWT, these data actually start 1968 instead of 1966
p_wst <- ggplot(wst, aes(x=Year, y=AvgCPUE))+
  geom_bar(stat="identity") +
  smr_theme() +
  theme(legend.position="none") + 
  scale_y_continuous("White Sturgeon CPUE") +
  smr_x_axis(report_year, type = "all", season = "fall") +
  stat_missing()+
  stat_lt_avg()

ggsave(p_wst, file="WST_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)
# ggsave(p_wst, file="WST_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")


#REcent trends for fish


#Delta Smelt

#Traditional Index 1967-2018
p_ds <- ggplot(ds, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
  smr_theme() +
  theme(legend.position="none") + 
  scale_y_continuous("Delta Smelt Index") +
  smr_x_axis(report_year, type = "recent", season = "fall") +
  stat_missing()+
  stat_lt_avg()
  
p_ds
ggsave(p_ds, file="FMWT_DS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)
# ggsave(p_ds, file="FMWT_DS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")


#Longfin Smelt

#Traditional Index 1967-2018
p_lfs <- ggplot(lfs, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
  smr_theme() +
  theme(legend.position="none") + 
  scale_y_continuous("Longfin Smelt Index") +
  smr_x_axis(report_year, type = "recent", season = "fall") +
  stat_missing()+
  stat_lt_avg()
p_lfs  

ggsave(p_lfs, file="FMWT_LFS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)



#Striped Bass

#Traditional Index 2004-20`9`
p_sb0 <- ggplot(sb0, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
  smr_theme() +
  theme(legend.position="none") + 
  scale_y_continuous("Striped Bass Index") +
  smr_x_axis(report_year, type = "recent", season = "fall") +
  stat_missing()+
  stat_lt_avg()

p_sb0
              
ggsave(p_sb0, file="FMWT_SB0_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)
# ggsave(p_sb0, file="FMWT_SB0_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")


#ggsave(p_sb0, file="sb0_trad_fmwt_color.png", path=plot_folder,scale=2,
#       dpi=300, units="cm",width=8.5,height=5.4)
#NOTE: need to specify path for plot_folder before saving


#American Shad

#Traditional Index 1967-2017
p_as <- ggplot(as, aes(x=Year, y=Index))+
  geom_bar(stat="identity") +
  smr_theme() +
  theme(legend.position="none") + 
  scale_y_continuous("American Shad Index") +
  smr_x_axis(report_year, type = "recent", season = "fall") +
  stat_missing()+
  stat_lt_avg()

p_as

ggsave(p_as, file="FMWT_AS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)
# ggsave(p_as, file="FMWT_AS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")




### Plot sturgeon  -------------------------------


#White Sturgeon
#unlike FMWT, these data actually start 1968 instead of 1966
p_wst <- ggplot(wst, aes(x=Year, y=AvgCPUE))+
  geom_bar(stat="identity") +
  smr_theme() +
  theme(legend.position="none") + 
  scale_y_continuous("White Sturgeon CPUE") +
  smr_x_axis(report_year, type = "recent", season = "fall") +
  stat_missing()+
  stat_lt_avg()

p_wst
ggsave(p_wst, file="WST_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)
# ggsave(p_wst, file="WST_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")
