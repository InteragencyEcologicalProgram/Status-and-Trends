#IEP Status and Trends Report
#Fall Season
#Data up to and including 2017
#Net Delta Outflow

#Created by Nick Rasmussen
#last updated: 3/15/2019

#packages
library(ggplot2) #plots
library(zoo)  # yearmon and yearqtr classes
library(tidyr) #separate one column into two
library(lubridate) #formatting dates

#import data
alldata<-read.csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/flow_1929-10-01_2018-09-30.csv") #daily outflow for full time series

#rename columns
names(alldata)<-c("date","out")

#look at data structure
alldata$date<-ymd(alldata$date) #format date
alldata$out<-as.numeric(alldata$out)
str(alldata)

#look at range of values for flow
range(alldata$out) #-37433 629494
range(alldata$date) #"1929-10-01" "2017-09-30"

#create a month column
alldata$month<-format(as.Date(alldata$date, format="%Y-%m-%d"),"%m")

#create a year column
alldata$year<-format(as.Date(alldata$date, format="%Y-%m-%d"),"%Y")

#now look at the series to dates to make sure they are correct
dseries<-unique(alldata$year) #looks good
alldata$month<-as.integer(alldata$month)
unique(alldata$month)

alldata$year<-as.integer(alldata$year)
str(alldata)

#combine month and year
alldata$ym <- as.yearmon(paste(alldata$month, alldata$year), "%m %Y")

#create a year-quarter column; the "+ 1/12" makes sure that December ends up as the first month of Q1 instead of Jan
alldata$yq <- as.yearqtr(alldata$ym + 1/12)

#date ranges based on quarters
range(alldata$yq) # "1929 Q4" "2017 Q4"

str(alldata)

#create columns that split the year and quarter into separate columns
#these will be used for plotting
alldata$yq2<-alldata$yq
alldata<-alldata %>% separate(yq2, c('qyear', 'quarter'), sep=" ")

alldata$qyear<-as.integer(alldata$qyear)
alldata$quarter<-factor(alldata$quarter)
str(alldata)

#count data points by quarter
table(alldata$quarter)

#generate means by year and season
dmean<-aggregate(out~qyear+quarter,data=alldata,FUN=mean,na.rm=T)
str(dmean)


#plots-----------------

#create custom plot formatting function
theme_iep <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, face = "plain"),
          axis.title.y = element_text(size = 14, face = "plain"
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


#create list of season names to replace quarter names in plots
#set up facet labels
season_names<-c('Q1'="Winter",'Q2'="Spring",'Q3'="Summer",'Q4'="Fall")

#set order of seasons for plotting
dmean$season = factor(dmean$quarter, levels=c('Q1','Q2','Q3','Q4'))

#plot data from all years with season faceted (so four plots to show all seasons)
ezq<-ggplot(dmean, aes(x=qyear, y=out)) +geom_line()+
  geom_point(colour="black") +
  facet_grid(season~.,labeller = as_labeller(season_names))+
  labs(x="Date", y="Flow (cfs)")+
  ggtitle("Net Delta Outflow Index")+theme_bw()+theme(plot.margin=margin(0.01,0.01,0.01,0.01,"in"));ezq
#ggsave("Flow_FacetedBySeason.png",type ="cairo-png",width=6.5, height=4.6,units="in",dpi=300)

wintermean = mean(filter(dmean, season == "Q1")$out)/1000
winter = ggplot(filter(dmean, season == "Q1"), aes(x=qyear, y=out/1000)) +
                  geom_line()+
  geom_point(colour="black") +
  geom_hline(aes(yintercept = wintermean), size = 0.9, color = "red", linetype = "dashed")+
  scale_x_continuous("Year (December - February)",limits = c(1966,2017))+
  scale_y_continuous(expression(paste("Net Delta Outflow (Ft "^"3"," / s x 1,000)"))) +
theme_bw()+theme(plot.margin=margin(0.01,0.01,0.01,0.01,"in"))
winter

ggsave(winter, file="winter_outflow_update.png", dpi=300, units="cm", width=9.3, height=6.8, path = "~/latex/figures")


#plot data from all years with just fall season 

#create subset with just fall
fall<-subset(dmean,quarter=="Q4")
str(fall)

#Plot of 1967 - 2017
(pfflow<-ggplot(fall, aes(x=qyear, y=out/1000)) +
    geom_line(colour="black", size=1.5)+geom_point(colour="black",size=2.8) +
    theme(legend.position="none")+ theme_iep()+
    scale_x_continuous("Year (September - November)",limits = c(1966,2017))+
    scale_y_continuous(expression(paste("Net Delta Outflow (Ft "^"3"," / s x 1,000)")), limits=c(0,max(fall$out)/1000))) 
#ggsave(pfflow, file="flow_fall.png", path=plot_folder,scale=1.8, dpi=300, units="cm",width=8.5,height=5.4)
#NOTE: need to set path for plot_folder before saving


#create plot with just 2002-2017
(pfflow2<-ggplot(fall, aes(x=qyear, y=out/1000)) +
    geom_line(colour="black", size=1.5)+geom_point(colour="black",size=2.8) +
    theme(legend.position="none")+ theme_iep()+
    scale_x_continuous("Year (September - November)",limits = c(2002,2017))+
    scale_y_continuous(expression(paste("Net Delta Outflow (Ft "^"3"," / s, ? 1,000)")), limits=c(0,13000/1000)))
#ggsave(pfflow2, file="flow_recent.png", path=plot_folder,scale=1.8, dpi=300, units="cm",width=8.5,height=5.4)
#NOTE: need to set path for plot_folder before saving


## Final figures:
flow_fig <- gridExtra::grid.arrange(pfflow, layout_matrix=rbind(1), 
												 heights=unit(102, c("mm")),
												 widths=unit(160, c("mm")))

flow2_fig <- gridExtra::grid.arrange(pfflow2, layout_matrix=rbind(1), 
												 heights=unit(102, c("mm")),
												 widths=unit(160, c("mm")))

flow_fig
flow2_fig
