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

#import data

#fall midwater trawl: Delta smelt, longfin smelt, striped bass, American shad
fmwt<-read.csv("data/fmwt.csv")
str(fmwt)

#remove years with "NAs" so that the missing data symbol works, also 
#make year a factor so it works with the standard x axis function
fmwt = filter(fmwt, !is.na(Index)) %>%
  mutate(fyear = as.factor(Year))

#CA Fish & Wildlife: Sturgeon Trammel Net Surveys
wst<-read.csv("data/wst.csv")
str(wst)
wst = mutate(wst, fyear = as.factor(Year))

### Plot Fall Midwater Trawl species -----------------------------------

#create subsets
ds<-subset(fmwt,Species=="Delta Smelt")
lfs<-subset(fmwt,Species=="Longfin Smelt")
sb0<-subset(fmwt,Species=="Striped Bass age-0")
as<-subset(fmwt,Species=="American Shad")
 

#Delta Smelt

#Traditional Index 1967-2018
p_ds <- ggplot(ds, aes(x=fyear, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() +
theme(legend.position="none") + 
  scale_y_continuous("Delta Smelt Index") +
  std_x_axis_all_years(2018) +
  std_x_axis_label("fall")+
  missing_data_symb(ds, fyear, 2018, 2)+
  lt_avg_line(mean(ds$Index, na.rm = T))
p_ds
ggsave(p_ds, file="FMWT_DS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")
ggsave(p_ds, file="FMWT_DS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")


#Longfin Smelt

#Traditional Index 1967-2018
p_lfs <- ggplot(lfs, aes(x=fyear, y=Index))+
    geom_bar(stat="identity") +
    theme_smr() +
    theme(legend.position="none") + 
  std_x_axis_label("fall")+
    scale_y_continuous("Longfin Smelt Index") +
  std_x_axis_all_years(2018) +
  missing_data_symb(lfs, fyear, 2018, 2)+
  lt_avg_line(mean(lfs$Index, na.rm = T))
p_lfs  
  
ggsave(p_lfs, file="FMWT_LFS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")
ggsave(p_lfs, file="FMWT_LFS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")



#Striped Bass

#Traditional Index 1967-2017
(p_sb0 <- ggplot(sb0, aes(x=fyear, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  scale_y_continuous("Striped Bass Index") +
  std_x_axis_all_years(2018) +
  missing_data_symb(sb0, fyear, 2018, 2))+
  std_x_axis_label("fall")+
  lt_avg_line(mean(sb0$Index, na.rm = T))
ggsave(p_sb0, file="FMWT_SB0_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")
ggsave(p_sb0, file="FMWT_SB0_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")


#ggsave(p_sb0, file="sb0_trad_fmwt_color.png", path=plot_folder,scale=2,
#       dpi=300, units="cm",width=8.5,height=5.4)
#NOTE: need to specify path for plot_folder before saving


#American Shad

#Traditional Index 1967-2017
(p_as <- ggplot(as, aes(x=fyear, y=Index))+
   geom_bar(stat="identity") +
   theme_smr() +
   theme(legend.position="none") + 
    scale_y_continuous("American Shad Index")+
    std_x_axis_all_years(2018) +
    std_x_axis_label("fall")+
    missing_data_symb(as, fyear, 2018, 2)+
  lt_avg_line(mean(as$Index, na.rm = T)))
ggsave(p_as, file="FMWT_AS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")
ggsave(p_as, file="FMWT_AS_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")




### Plot sturgeon  -------------------------------


#White Sturgeon
#unlike FMWT, these data actually start 1968 instead of 1966
(p_wst <- ggplot(wst, aes(x=fyear, y=AvgCPUE))+
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
   std_x_axis_label("fall")+
  scale_y_continuous("White Sturgeon CPUE")) +
  std_x_axis_all_years(2018) +
  missing_data_symb(wst, fyear, 2018, 2)+
  lt_avg_line(mean(wst$AvgCPUE, na.rm = T))
ggsave(p_wst, file="WST_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")
ggsave(p_wst, file="WST_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")


#REcent trends for fish


#Delta Smelt

#Traditional Index 1967-2018
p_ds <- ggplot(ds, aes(x=fyear, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  std_x_axis_label("fall")+
  scale_y_continuous("Delta Smelt Index") +
  std_x_axis_rec_years(2018) +
  missing_data_symb(ds, fyear, 2018, 2)+
  lt_avg_line(mean(ds$Index, na.rm = T))
p_ds
ggsave(p_ds, file="FMWT_DS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")
ggsave(p_ds, file="FMWT_DS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")


#Longfin Smelt

#Traditional Index 1967-2018
p_lfs <- ggplot(lfs, aes(x=fyear, y=Index))+
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  std_x_axis_label("fall")+
  scale_y_continuous("Longfin Smelt Index") +
  std_x_axis_rec_years(2018) +
  missing_data_symb(lfs, fyear, 2018, 2)+
  lt_avg_line(mean(lfs$Index, na.rm = T))
p_lfs  

ggsave(p_lfs, file="FMWT_LFS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")
ggsave(p_lfs, file="FMWT_LFS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")



#Striped Bass

#Traditional Index 1967-2017
(p_sb0 <- ggplot(sb0, aes(x=fyear, y=Index))+
    geom_bar(stat="identity") +
    theme_smr() +
    theme(legend.position="none") + 
    scale_y_continuous("Striped Bass Index") +
    std_x_axis_rec_years(2018) +
    std_x_axis_label("fall")+
    missing_data_symb(sb0, fyear, 2018, 2))+
  lt_avg_line(mean(sb0$Index, na.rm = T))
ggsave(p_sb0, file="FMWT_SB0_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")
ggsave(p_sb0, file="FMWT_SB0_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")


#ggsave(p_sb0, file="sb0_trad_fmwt_color.png", path=plot_folder,scale=2,
#       dpi=300, units="cm",width=8.5,height=5.4)
#NOTE: need to specify path for plot_folder before saving


#American Shad

#Traditional Index 1967-2017
(p_as <- ggplot(as, aes(x=fyear, y=Index))+
    geom_bar(stat="identity") +
    theme_smr() +
    theme(legend.position="none") + 
    std_x_axis_label("fall")+
    scale_y_continuous("American Shad Index")+
    std_x_axis_rec_years(2018) +
    missing_data_symb(as, fyear, 2018, 2)+
    lt_avg_line(mean(as$Index, na.rm = T)))
ggsave(p_as, file="FMWT_AS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")
ggsave(p_as, file="FMWT_AS_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")




### Plot sturgeon  -------------------------------


#White Sturgeon
#unlike FMWT, these data actually start 1968 instead of 1966
(p_wst <- ggplot(wst, aes(x=fyear, y=AvgCPUE))+
   geom_bar(stat="identity") +
   theme_smr() +
   std_x_axis_label("fall")+
   theme(legend.position="none") + 
   scale_y_continuous("White Sturgeon CPUE")) +
  std_x_axis_rec_years(2018) +
  missing_data_symb(wst, fyear, 2018, 2)+
  lt_avg_line(mean(wst$AvgCPUE, na.rm = T))
ggsave(p_wst, file="WST_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "Fall_report/figures")
ggsave(p_wst, file="WST_2004.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")
