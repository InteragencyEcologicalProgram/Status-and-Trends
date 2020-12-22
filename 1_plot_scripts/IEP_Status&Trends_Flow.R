#IEP Status and Trends Report
#all seasons
#Data up to and including 2018
#Net Delta Outflow

#Created by Nick Rasmussen
#last updated: 11/3/2020 by Rosemary Hartman

#packages
library(ggplot2) #plots
library(zoo)  # yearmon and yearqtr classes
library(tidyr) #separate one column into two
library(lubridate) #formatting dates
library(smonitr)

#source(file.path(data_access_root,"data_access_dayflow.R"))
alldata = read.csv(file.path(data_root,"dayflow_all.csv"))
#rename columns
names(alldata)<-c("date","out")

#reformat date
alldata$date<-ymd(alldata$date) #format date

#create a month column, year, and quarter (putting december in first month of Q1)
alldata = mutate(alldata, month = month(date),
                 year = year(date),
                 ym = as.yearmon(paste(month, year), "%m %Y"),
                 yq = as.yearqtr(ym + 1/12), yq2 = yq) %>%
                 separate(yq2, c('qyear', 'quarter'), sep=" ") %>%
  mutate(qyear = as.integer(qyear), quarter = factor(quarter))


#generate means by year and season
dmean<-aggregate(out~qyear+quarter,data=alldata,FUN=mean,na.rm=T)
str(dmean)


#plots-----------------


#create list of season names to replace quarter names in plots
#set up facet labels
season_names<-c('Q1'="winter",'Q2'="spring",'Q3'="summer",'Q4'="fall")

#set order of seasons for plotting
dmean$season = factor(dmean$quarter, levels=c('Q1','Q2','Q3','Q4'))

#plot data from all years with season faceted (so four plots to show all seasons)
ezq<-ggplot(dmean, aes(x=qyear, y=out)) +geom_line()+
  geom_point(colour="black") +
  facet_grid(season~.,labeller = as_labeller(season_names))+
  labs(x="Date", y="Flow (cfs)")+
  ggtitle("Net Delta Outflow Index")+theme_bw()+theme(plot.margin=margin(0.01,0.01,0.01,0.01,"in"));ezq
#ggsave("Flow_FacetedBySeason.png",type ="cairo-png",width=6.5, height=4.6,units="in",dpi=300)


#now a function to plot one season at a time
flows = function(quart, data) {
  dat = filter(data, quarter == quart) 
  p = ggplot(dat, aes(x=qyear, y=out/1000)) +
    geom_line()+
    geom_point(colour="black") +
    stat_lt_avg(aes(y = out/1000))+
  #  geom_hline(aes(yintercept = mean(out)/1000), size = 0.9, color = "red", linetype = "dashed")+
    smr_x_axis(report_year, type = "all", season = season_names[quart]) + 
    scale_y_continuous(limits = c(0, max(data$out/1000)), name =expression(paste("Net Delta Outflow (1,000 Ft "^"3"," / s)"))) +
    smr_theme()
  p
}

flows(quart = "Q1",data = dmean)
flows(quart = "Q3",data = dmean)


ggsave(flows(quart = "Q1",data = dmean), 
       file="winter_outflow_update.png", 
       dpi=300, units="cm", width=9.3, height=6.8,
       path = fig_root_winter)

ggsave(flows(quart = "Q2",data = dmean), 
       file="spring_outflow_update.png", 
       dpi=300, units="cm", width=9.3, height=6.8,
       path = fig_root_spring)

ggsave(flows(quart = "Q3",data = dmean), 
       file="summer_outflow_update.png", 
       dpi=300, units="cm", width=9.3, height=6.8,
       path = fig_root_summer)

ggsave(flows(quart = "Q4",data = dmean), 
       file="fall_outflow_update.png", 
       dpi=300, units="cm", width=9.3, height=6.8,
       path = fig_root_fall)

ggsave(flows(quart = "Q4",data = dmean), 
       file="fall_outflow_update.png", 
       dpi=300, units="cm", width=9.3, height=6.8,
       path = fig_root_fall)
