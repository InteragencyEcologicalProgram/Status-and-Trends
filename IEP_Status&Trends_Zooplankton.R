#IEP Status and Trends Report
#all seasons
#Data up to and including 2018
#Zooplankton Biomass Per Unit Effort

#Created by Nick Rasmussen
#last updated: 1/17/2020

#Clark-Bumpus Net: Calanoid copepods, Cladocerans, Cyclopoids (Acanthocyclops and other cyclopoids)
#Rotifer pump: Cyclopoids (Oithona, Limnoithona)
#Mysid net: all species

#packages
library(tidyr) #convert df from wide to long
library(ggplot2)
library(colorspace) #color palette
library(zoo) ## yearmon and yearqtr classes
library(cowplot)
library(tidyverse)
library(lubridate)
source("drivr_sqlite.R")

#import datasets----------------

#Clark-Bumpus net survey data
zoopcb<-zoopsql 
zoopcb$Date = as.Date(zoopcb$Date)

#mysid net survey data
mysid<-read.csv("./data/zoop_mysid.csv") 

#details of sampling station locations
station<-read.csv("./data/zoop_stations.csv") 

#carbon mass for individual zooplankton taxa (cladocerans, copepods)
zmass<-read.csv("./data/zoop_individual_mass.csv") 

#mysid shrimp biomass for all samples collected 
mmass<-read.csv("./data/zoop_mysid_mass.csv") 


#CB net: formatting data---------
#reduce count and biomass data to just needed elements and then use to calculate biomass

names(zoopcb)
#str(zoopcb)

#exclude unneeded rows:
#exclude EZ stations (NZEZ2, NZEZ6)
#exclude non-core stations (core = 0); 1 = sampled since 1972, 2 = sampled since 1974
#exclude 1972-1973 because only subset of core stations were sampled during those years
#exclude survey replicate 2 for the months that have those; vast majority of monthly surveys are just one replicate
#exclude the winter months (1,2,12) initially because they have a shorter time series than the other months
zoop_sub<-subset(zoopcb, Station!="NZEZ2" & Station!="NZEZ6" & Core!=0 & Year>1973 & SurveyRep!=2 & Survey>2 & Survey<12)

#create separate subset of the two san pablo stations to include in graphs (no core stations in SP)
zoop_subsp<-subset(zoopcb, Station=="NZD41" | Station=="NZ41A")
unique(zoop_subsp$SurveyRep) #only includes one sample per month
range(zoop_subsp$Year) #starts after 1998

#create separate subset for winter months with its shorter time series
zoop_subw<-subset(zoopcb, Station!="NZEZ2" & Station!="NZEZ6" & Core!=0 & Year>1993 & SurveyRep!=2 & (Survey<3 | Survey>11))

unique(zoop_subw$Station)

#combine the three data subsets
zoop_s<-rbind(zoop_sub,zoop_subsp,zoop_subw)

#reduce zooplankton count data to just needed columns
zoop<-subset(zoop_s,select=c("Year","Survey","Date","Station","ACARTELA","ACARTIA","DIAPTOM","EURYTEM","OTHCALAD","PDIAPFOR"
                             ,"PDIAPMAR","SINOCAL","TORTANUS","AVERNAL","OTHCYCAD","BOSMINA","DAPHNIA","DIAPHAN","OTHCLADO")  )

#convert zoop data frame from wide to long format
zoopl<-gather(zoop,taxon,cpue,ACARTELA:OTHCLADO)

#modify column headers
names(zoopl)[1:4]<-c("year","survey","date","station")

#format some columns
zoopl$taxon<-factor(zoopl$taxon)

#add in individual biomass data
zoopb<-left_join(zoopl,zmass,by="taxon")

#calculate biomass by sample-taxon combo
zoopb$bpue<-zoopb$cpue*zoopb$mass_indiv_ug
#create new column for zooplankton categories
#calanoids: "ACARTELA","ACARTIA","DIAPTOM","EURYTEM","OTHCALAD","PDIAPFOR","PDIAPMAR","SINOCAL","TORTANUS"
#cyclopoids: "AVERNAL","OTHCYCAD","LIMNOSINE","LIMNOSPP","LIMNOTET" , "OITHDAV" , "OITHSIM" , "OITHSPP"
#cladocerans: "BOSMINA","DAPHNIA","DIAPHAN","OTHCLADO"

#duplicate the taxon column to make a higher and lower taxon column
zall = mutate(zoopb, taxonl = taxon)

#first, create vectors of strings to replace with broader category names 
calan<-c("ACARTELA","ACARTIA","DIAPTOM","EURYTEM","OTHCALAD","PDIAPFOR","PDIAPMAR","SINOCAL","TORTANUS")
cyclop<-c("AVERNAL","OTHCYCAD","LIMNOSINE","LIMNOSPP","LIMNOTET" , "OITHDAV" , "OITHSIM" , "OITHSPP")
cladoc<-c("BOSMINA","DAPHNIA","DIAPHAN","OTHCLADO")


#calanoids: use a loop to find those strings and replace them with "calan" 
for(sp in calan){
  zall$taxon = gsub(sp,"cala",zall$taxon)
}

#cyclopoids: use a loop to find those strings and replace them with "cycl" 
for(sp in cyclop){
  zall$taxon = gsub(sp,"cycl",zall$taxon)
}

#cladocerans: use a loop to find those strings and replace them with "clad" 
for(sp in cladoc){
  zall$taxon = gsub(sp,"clad",zall$taxon)
}

unique(zall$taxon) #"cala" "cycl" "clad"
#looks like it worked

zall$taxon<-factor(zall$taxon)
str(zall)

#add together biomass for all species within each of the three taxonomic groups
zll<- group_by(zall, year, survey, date, station, taxon) %>%
  summarize(cpue = sum(cpue, na.rm = T), bpue = sum(bpue, na.rm = T))

#mysids: formatting data---------

#start with cpue data
names(mysid)

#exclude unneeded rows:
#exclude EZ stations (NZEZ2, NZEZ6)
#exclude non-core stations, called 'Index' instead of core (core = 0); 1 = sampled since 1972, 2 = sampled since 1974
#exclude 1972-1973 because only subset of core stations were sampled during those years
#exclude survey replicate 2 for the months that have those; vast majority of monthly surveys are just one replicate
#exclude the winter months (1,2,12) initially because they have a shorter time series than the other months
zoop_sub3<-subset(mysid, Station!="NZEZ2" & Station!="NZEZ6" & Index!=0 & Year>1973 & SurveyRep!=2 & Survey>2 & Survey<12)

#create separate subset of the two san pablo stations to include in graphs (no core stations in SP)
zoop_subsp3<-subset(mysid, Station=="NZD41" | Station=="NZ41A")

#create separate subset for winter months with its shorter time series
zoop_subw3<-subset(mysid, Station!="NZEZ2" & Station!="NZEZ6" & Index!=0 & Year>1993 & SurveyRep!=2 & (Survey<3 | Survey>11))

#combine the three data subsets
zoop_s3<-rbind(zoop_sub3,zoop_subsp3,zoop_subw3)

#create column that adds up counts of all mysid species
zoop_s3$cpue<-rowSums(zoop_s3[25:32])

#reduce zooplankton data to just needed columns
#"Year","Survey","Date","Station", "cpue"
zoop3<-subset(zoop_s3,select=c("Year","Survey","Date","Station","cpue"))

#next format the bpue dataset

#add up bpue across species
mmass$bpue_mg<-rowSums(mmass[20:27])

#create column with bpue in micrograms instead of milligrams because rest of zoop is in micrograms
mmass$bpue<-mmass$bpue*1000

#reduce bpue data set to just needed columns
zoop3b<-subset(mmass,select=c("Year","Survey","Date","Station","bpue"))

#combine cpue and bpue data sets
str(zoop3)
str(zoop3b)
zoop3m<-left_join(zoop3,zoop3b,by=c("Year","Survey","Date","Station"))

names(zoop3m)<-c("year","survey","date","station","cpue","bpue")

#add a taxon column so that this data frame can be combined with the others
zoop3m$taxon<-"mys"
zoop3m$taxon<-zoop3m$taxon
names(zoop3m)

#format date
zoop3m$date<-as.Date(zoop3m$date,format="%m/%d/%Y")

#combine all zoop data sets
mza<-bind_rows(zll,zoop3m)


#Stations: formatting data---------

names(station)

#combine deg, min, zoop for lat and for long

#first calculate decimal deg, min, sec
station$long_dec<-station$long_deg + station$long_min/60 + station$long_sec/3600

#reduce to just needed columns
stati<-subset(station,select=c("station","long_dec"))
str(stati)
#sort by longitude
stati<-stati[order(stati$long_dec),]

#merge with zooplankton cpue data
mcpg<-left_join(mza, stati)

unique(mcpg$station) #18 stations: 16 core stations + 2 san pablo stations

#add column for geographic region based on longitude
mcpg$region<-ifelse(mcpg$long_dec > 122.216, "spl", 
                    ifelse(mcpg$long_dec < 122.216 & mcpg$long_dec > 121.829, "ss",
                           ifelse(mcpg$long_dec < 121.829, "dt",NA))) 

#make region a factor
mcpg$region<-factor(mcpg$region, levels=c('spl','ss','dt'))

#add column for season
#a bit tricky because winter is Dec. of one year and then Jan./Feb. of the following year
#https://stackoverflow.com/questions/41234275/calculate-seasonal-mean-with-a-n-years-time-series-with-monthly-data?rq=1
#use survey number instead of trying to make and use a month column

#combine month and year
mcpg = ungroup(mcpg) %>%
  mutate( month = month(date), 
              ym =as.yearmon(paste(month, year), format =  "%m %Y"),
              yq =as.yearqtr(ym + 1/12, format = "%Y Q%q"),
              yq2 = yq) %>%
  separate(yq2, c('qyear', 'quarter'), sep=" ") %>%
  mutate(quarter = factor(quarter, levels=c('Q1','Q2','Q3','Q4')),
         qyear = as.integer(qyear))


#generate means by region, year, quarter, and taxon
zmeans<-aggregate(cbind(cpue,bpue)~region+qyear+quarter+taxon,data=mcpg,FUN=mean,na.rm=T)

zmeans = group_by(mcpg, region, qyear, quarter, taxon) %>%
  summarise(cpue = mean(cpue), bpue = mean(bpue))
str(zmeans)



#CPUE plots: stacked line plots for each season and region------------

#create custom plot formatting function
source("IEP_Plot_Theme.R")

#set up facet labels
season_names<-c('Q1'="Winter",'Q2'="Spring",'Q3'="Summer",'Q4'="Fall")
region_names<-c('dt'="Delta",'ss'="Suisun",'spl'="San Pablo")

#custom colors
(custcol<-diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))
#"#664F2B" "#928678" "#748D83" "#095E49"


#BPUE plots: stacked line plots for each season and region------------



#All zoop BPUE (ug): facets of stacked line plots
(bpl<-ggplot(zmeans, aes(x = qyear, y = bpue, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (",mu,"g C/m"^" 3", ")")), limits=c(0,max(zmeans$bpue))))
#NOTE: mysids overwhelm everything else because they are so much larger

zoops = function(reg, quart, data) {
  dat = filter(data, quarter == quart, region == reg)
  
  #calculate long-term average
  sums = group_by(dat, qyear) %>% summarize(bpuetot = sum(bpue))
  meanB = mean(sums$bpuetot)
  
  #make a plot
  bpl<-ggplot(dat, aes(x = qyear, y = bpue, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018))  +
    geom_hline(aes(yintercept = meanB), size = 0.9, color = "red", linetype = "dashed")+
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (",mu,"g C/m"^" 3", ")")), 
                       limits=c(0,max(zmeans$bpue)))
  bpl


}


#Let's try one that puts all the regions together

zoops2 = function(quart, data) {
  dat = filter(data, quarter == quart)
  
  #calculate long-term average
  sums = group_by(dat, region, qyear) %>% summarize(bpuetot = sum(bpue))
  meanB = group_by(sums, region) %>% summarize(bpueM =mean(bpuetot))
  
  #make a plot
  bpl<-ggplot(dat, aes(x = qyear, y = bpue, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+ facet_wrap(~region) +
    theme(legend.position="top", legend.margin = margin(0,0,0,0) , 
          strip.background = element_blank(),
          strip.text = element_blank()) + 
    scale_x_continuous(paste("Year(", season_names[unlist(dat[1,"quarter"])],")"), limits=c(1966,2018))  +
    geom_hline(data = meanB, aes(yintercept = bpueM), size = 0.9, color = "red", linetype = "dashed")+
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (",mu,"g C/m"^" 3", ")")), 
                       limits=c(0,max(zmeans$bpue)))
  bpl
  
  
}

zmeans = filter(zmeans, qyear <= 2018)

ggsave(zoops2("Q3", zmeans), file="zoops_panel_summer.png", 
       dpi=300, units="cm",width=27.9,height=7.5,
       path = "./summer_report")

ggsave(zoops2("Q2", zmeans), file="zoops_panel_spring.png", dpi=300, units="cm",width=27.9,height=7.5,
       path = "./spring_report")

ggsave(zoops2("Q1", zmeans), file="zoops_panel_winter.png", dpi=300, units="cm",width=27.9,height=7.5,
       path = "./winter_report")

ggsave(zoops2("Q4", zmeans), file="zoops_panel_fall.png", 
       dpi=300, units="cm",width=27.9,height=7.5,
       path = "./fall_report")





#####################################################################################
#this is the old version that grobs them together, but i need to do something about the legend.


#winter zoops plot
zoopsw<-plot_grid(zoops("spl", "Q1", zmeans),
                 zoops("ss", "Q1", zmeans), 
                 zoops("dt", "Q1", zmeans),
                 ncol = 3, nrow = 1, align="v")


#spring zoops plot
zoopsf<-plot_grid(zoops("spl", "Q2", zmeans),
                 zoops("ss", "Q2", zmeans),
                 zoops("spl", "Q2", zmeans),
                 ncol = 3, nrow = 1, align="v")
zoopsf


#summer chla plot
zoopss<-plot_grid(zoops("spl", "Q3", zmeans),
                 zoops("ss", "Q3", zmeans),
                 zoops("spl", "Q3", zmeans),
                 ncol = 3, nrow = 1, align="v")

#fall chlae plot
zoopsf<-plot_grid(zoops("spl", "Q4", zmeans),
                 zoops("ss", "Q4", zmeans),
                 zoops("spl", "Q4", zmeans),
                 ncol = 3, nrow = 1, align="v")
zoopsf
