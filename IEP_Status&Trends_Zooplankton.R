#IEP Status and Trends Report
#Fall Season
#Data up to and including 2017
#Zooplankton Biomass Per Unit Effort

#Created by Nick Rasmussen
#last updated: 3/18/2019

#Clark-Bumpus Net: Calanoid copepods, Cladocerans, Cyclopoids (Acanthocyclops and other cyclopoids)
#Rotifer pump: Cyclopoids (Oithona, Limnoithona)
#Mysid net: all species

#packages
library(plyr) #join()
library(tidyr) #convert df from wide to long
library(ggplot2)
library(colorspace) #color palette
library(zoo) ## yearmon and yearqtr classes
library(cowplot)


#import datasets----------------

#Clark-Bumpus net survey data
zoopcb<-read.csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/zoop_cb.csv") 

#rotifer pump survey data
zoopp<-read.csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/zoop_pump.csv") 

#mysid net survey data
mysid<-read.csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/zoop_mysid.csv") 

#details of sampling station locations
station<-read.csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/zoop_stations.csv") 

#carbon mass for individual zooplankton taxa (cladocerans, copepods)
zmass<-read.csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/zoop_individual_mass.csv") 

#mysid shrimp biomass for all samples collected 
mmass<-read.csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/zoop_mysid_mass.csv") 

#chlorophyll-a data; used to create chl-a/zoop plot panel for report
chlora<-read.csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/Status-and-Trends/master/chla_all_seasons.csv") 


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
zoopl$date<-as.Date(zoopl$date,format="%m/%d/%Y")
zoopl$taxon<-factor(zoopl$taxon)
str(zoopl)
str(zmass)

#add in individual biomass data
zoopb<-join(zoopl,zmass,by="taxon",type="left")

#calculate biomass by sample-taxon combo
zoopb$bpue<-zoopb$cpue*zoopb$mass_indiv_ug

#plot cpue vs bpue as a quick check
plot(zoopb$cpue,zoopb$bpue)

#look at date ranges of data sets
range(zoopb$date) #"1974-03-13" "2017-12-15", good - it ends at same date as WQ data I'm using

#Rotifer pump: formatting data---------

names(zoopp)

#exclude unneeded rows:
#exclude EZ stations (NZEZ2, NZEZ6)
#exclude non-core stations (core = 0); 1 = sampled since 1972, 2 = sampled since 1974
#exclude 1972-1973 because only subset of core stations were sampled during those years
#exclude survey replicate 2 for the months that have those; vast majority of monthly surveys are just one replicate
#exclude the winter months (1,2,12) initially because they have a shorter time series than the other months
zoop_sub2<-subset(zoopp, Station!="NZEZ2" & Station!="NZEZ6" & Core!=0 & Year>1973 & SurveyRep!=2 & Survey>2 & Survey<12)

#create separate subset of the two san pablo stations to include in graphs (no core stations in SP)
zoop_subsp2<-subset(zoopp, Station=="NZD41" | Station=="NZ41A")

#create separate subset for winter months with its shorter time series
zoop_subw2<-subset(zoopp, Station!="NZEZ2" & Station!="NZEZ6" & Core!=0 & Year>1993 & SurveyRep!=2 & (Survey<3 | Survey>11))

#combine the three data subsets
zoop_s2<-rbind(zoop_sub2,zoop_subsp2,zoop_subw2)

#reduce zooplankton data to just needed columns
#"Year","Survey","SampleDate","Station" 
#"LIMNOSINE"+"LIMNOSPP"+"LIMNOTET" + "OITHDAV" + "OITHSIM" + "OITHSPP" = the two cyclopoid genera best surveyed by pump
zoop2<-subset(zoop_s2,select=c("Year","Survey","SampleDate","Station","LIMNOSINE","LIMNOSPP","LIMNOTET","OITHDAV","OITHSIM","OITHSPP"))

#convert zoop data frame from wide to long format
zoopl2<-gather(zoop2,taxon,cpue,LIMNOSINE:OITHSPP)

#name columns
names(zoopl2)[1:4]<-c("year","survey","date","station")

#format some columns
zoopl2$date<-as.Date(zoopl2$date,format="%m/%d/%Y")
zoopl2$taxon<-factor(zoopl2$taxon)
zoopl2$cpue<-as.numeric(zoopl2$cpue)
str(zoopl2)
str(zmass)

#add in individual biomass data
zoopb2<-join(zoopl2,zmass,by="taxon",type="left")

#calculate biomass by sample-taxon combo
zoopb2$bpue<-zoopb2$cpue*zoopb2$mass_indiv_ug

#plot cpue vs bpue as a quick check
plot(zoopb2$cpue,zoopb2$bpue)

#look at date ranges of data sets
range(zoopb2$date) #"1974-03-13" "2017-12-15", good - it ends at same date as WQ data I'm using


#combine the CB and pump data sets
names(zoopb)
names(zoopb2)
str(zoopb)
str(zoopb2)
zall<-rbind(zoopb,zoopb2)

#create new column for zooplankton categories
#calanoids: "ACARTELA","ACARTIA","DIAPTOM","EURYTEM","OTHCALAD","PDIAPFOR","PDIAPMAR","SINOCAL","TORTANUS"
#cyclopoids: "AVERNAL","OTHCYCAD","LIMNOSINE","LIMNOSPP","LIMNOTET" , "OITHDAV" , "OITHSIM" , "OITHSPP"
#cladocerans: "BOSMINA","DAPHNIA","DIAPHAN","OTHCLADO"

#duplicate the taxon column to make a higher and lower taxon column
zall$taxonl<-zall$taxon

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
zll<-aggregate(cbind(cpue,bpue)~year+survey+date+station+taxon,data=zall,FUN=sum,na.rm=T)


#reduce to just needed columns
zal<-subset(zll,select=c("year","survey","date","station","taxon","cpue","bpue"))


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
mmass$bpue_mg<-rowSums(mmass[25:32])

#create column with bpue in micrograms instead of milligrams because rest of zoop is in micrograms
mmass$bpue<-mmass$bpue*1000

#reduce bpue data set to just needed columns
zoop3b<-subset(mmass,select=c("Year","Survey","Date","Station","bpue"))

#combine cpue and bpue data sets
str(zoop3)
str(zoop3b)
zoop3m<-join(zoop3,zoop3b,by=c("Year","Survey","Date","Station"),type="left")

names(zoop3m)<-c("year","survey","date","station","cpue","bpue")

#add a taxon column so that this data frame can be combined with the others
zoop3m$taxon<-"mys"
zoop3m$taxon<-factor(zoop3m$taxon)
names(zoop3m)

#reorder columns
zoopl3<-zoop3m[c(1:4,7,5:6)]

#format date
zoopl3$date<-as.Date(zoopl3$date,format="%m/%d/%Y")
str(zoopl3)

#look at date ranges of data sets
range(zoopl3$date) #"1974-03-13" "2017-12-15", good - it ends at same date as WQ data I'm using

#then combine with rest of zooplankton data
names(zal)
str(zal)
names(zoopl3)

#combine all zoop data sets
mza<-rbind(zal,zoopl3)


#Stations: formatting data---------

names(station)

#combine deg, min, sec for lat and for long

#first calculate decimal deg, min, sec
station$long_dec<-station$long_deg + station$long_min/60 + station$long_sec/3600

#reduce to just needed columns
stati<-subset(station,select=c("station","long_dec"))
str(stati)
#sort by longitude
stati<-stati[order(stati$long_dec),]

#merge with zooplankton cpue data
mcpg<-join(mza,stati,type="left")

unique(mcpg$station) #18 stations: 16 core stations + 2 san pablo stations

#add column for geographic region based on longitude
mcpg$region<-ifelse(mcpg$long_dec > 122.216, "spl", 
                    ifelse(mcpg$long_dec < 122.216 & mcpg$long_dec > 121.829, "ss",
                           ifelse(mcpg$long_dec < 121.829, "dt",NA))) 

#make region a factor
mcpg$region<-factor(mcpg$region)


#look at stations within each region
west<-subset(mcpg,region=="spl")
unique(west$station)

midd<-subset(mcpg,region=="ss")
unique(midd$station)

east<-subset(mcpg,region=="dt")
unique(east$station)

#count data points by region
table(mcpg$region)


#add column for season
#a bit tricky because winter is Dec. of one year and then Jan./Feb. of the following year
#https://stackoverflow.com/questions/41234275/calculate-seasonal-mean-with-a-n-years-time-series-with-monthly-data?rq=1
#use survey number instead of trying to make and use a month column

#combine month and year
mcpg$ym <- as.yearmon(paste(mcpg$survey, mcpg$year), "%m %Y")

#create a year quarter column; the "+ 1/12" makes sure that December ends up as the first month of Q1 instead of Jan
mcpg$yq <- as.yearqtr(mcpg$ym + 1/12)

#date ranges based on quarters
range(mcpg$yq) # "1974 Q2" "2018 Q1"

str(mcpg)

#create columns that split the year and quarter into separate columns
#these will be used for plotting
mcpg$yq2<-mcpg$yq
mcpg<-mcpg %>% separate(yq2, c('qyear', 'quarter'), sep=" ")
mcpg$year<-as.integer(mcpg$year)


#count data points by season
table(mcpg$quarter)
#winter (Q1) season has almost half as many data points as other seasons which probably makes sense
str(mcpg)

#make quarter a factor and year an integer
mcpg$quarter<-factor(mcpg$quarter)
mcpg$qyear<-as.integer(mcpg$qyear)


#generate means by region, year, quarter, and taxon
zmeans<-aggregate(cbind(cpue,bpue)~region+qyear+quarter+taxon,data=mcpg,FUN=mean,na.rm=T)
str(zmeans)

#export summary data 
#write.csv(zmeans,"zoop_bpue_means.csv", row.names=F)

#quick summary stats
range(zmeans$cpue)
range(zmeans$bpue)


#create subset without mysids
cbp<-subset(zmeans,taxon!="mys")

#create subset with only mysids
mys<-subset(zmeans,taxon=="mys")


#CPUE plots: stacked line plots for each season and region------------

#create custom plot formatting function
theme_iep <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"
                                      ,margin=margin(t = 0, r = 10, b = 0, l = 0)
          ),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          #plot.margin = unit(c(0.1, 0.3, 0.1, 0.9), units = , "cm"), #top, right, bottom, left
          plot.margin = unit(c(0.2, 0.5, 0.1, 0.9), units = , "cm"), #adjusted the "top" and "right" values so nothing is cut off
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "plain"),
          legend.title=element_text(size=14))
}

#set up facet labels
season_names<-c('Q1'="Winter",'Q2'="Spring",'Q3'="Summer",'Q4'="Fall")
region_names<-c('dt'="Delta",'ss'="Suisun",'spl'="San Pablo")

#set order of seasons and regions for plotting
zmeans$quarter = factor(zmeans$quarter, levels=c('Q1','Q2','Q3','Q4'))
zmeans$region = factor(zmeans$region, levels=c('spl','ss','dt'))

cbp$quarter = factor(cbp$quarter, levels=c('Q1','Q2','Q3','Q4'))
cbp$region = factor(cbp$region, levels=c('spl','ss','dt'))

mys$quarter = factor(mys$quarter, levels=c('Q1','Q2','Q3','Q4'))
mys$region = factor(mys$region, levels=c('spl','ss','dt'))

#custom colors
(custcol<-diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))
#"#664F2B" "#928678" "#748D83" "#095E49"

#All zoop CPUE: facets of stacked line plots
(cpl<-ggplot(zmeans, aes(x = qyear, y = cpue, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Density (Number/m"^" 3", ")")), limits=c(0,max(zmeans$cpue))))
#NOTE: mysids are effectively invisible because there are so few of them per sample; would need to use a log scale for them to show up

#ggsave(cpl, file="zoop_cpue_all.png",scale=1,dpi=300, units="cm",width=30,height=20.5)

#Zoop CPUE, no mysids: facets of stacked line plots 
(cplnm<-ggplot(cbp, aes(x = qyear, y = cpue, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids")
                      ,values=c("#664F2B","#928678","#748D83"))+
                    scale_y_continuous(expression(paste("Zooplankton Density (Number/m"^" 3", ")")), limits=c(0,max(cbp$cpue))))
 
#ggsave(cplnm, file="zoop_cpue_no_mysids.png",scale=1,dpi=300, units="cm",width=30,height=20.5)


#Mysids only CPUE: facets of stacked line plots 
(cplm<-ggplot(mys, aes(x = qyear, y = cpue, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Mysids"),values="#095E49")+
    scale_y_continuous(expression(paste("Zooplankton Density (Number/m"^" 3", ")")), limits=c(0,max(mys$cpue))))

#ggsave(cplm, file="zoop_cpue_only_mysids.png",scale=1,dpi=300, units="cm",width=30,height=20.5)


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

#All zoop BPUE (mg): facets of stacked line plots
(bpl2<-ggplot(zmeans, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(zmeans$bpue)/1000)
                       ))
#NOTE: mysids overwhelm everything else because they are so much larger

#ggsave(bpl2, file="zoop_bpue_all.png",scale=1,dpi=300, units="cm",width=30,height=20.5)


#Zoop BPUE, no mysids (mg): facets of stacked line plots   
(bplnm<-ggplot(cbp, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids")
                      ,values=c("#664F2B","#928678","#748D83"))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(cbp$bpue)/1000)
                       ))

#ggsave(bplnm, file="zoop_bpue_no_mysids.png",scale=1,dpi=300, units="cm",width=30,height=20.5)


#Mysids only BPUE: facets of stacked line plots  
(bplm<-ggplot(mys, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Mysids"),values="#095E49")+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(mys$bpue)/1000)
                       ))

#ggsave(bplm, file="zoop_bpue_only_mysids.png",scale=1,dpi=300, units="cm",width=30,height=20.5)


#BPUE, all zooplankton categories, each season is separate panel-----------------

bw<-subset(zmeans,quarter=="Q1")
bsp<-subset(zmeans,quarter=="Q2")
bsu<-subset(zmeans,quarter=="Q3")
bf<-subset(zmeans,quarter=="Q4")


#Winter, all zoop BPUE (mg): facets of stacked line plots
(bwp<-ggplot(bw, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(bw$bpue/1000))  #this seems to mess up line for calanoids
                       ))

#ggsave(bwp, file="zoop_bpue_all_winter.png",scale=2,dpi=300, units="cm",width=30,height=7)


#Spring, all zoop BPUE (mg): facets of stacked line plots
(bspp<-ggplot(bsp, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(bsp$bpue)/1000)
                       ))

#ggsave(bspp, file="zoop_bpue_all_spring.png",scale=2,dpi=300, units="cm",width=30,height=7)


#Summer, all zoop BPUE (mg): facets of stacked line plots
(bsup<-ggplot(bsu, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(bsu$bpue)/1000)
                       ))

#ggsave(bsup, file="zoop_bpue_all_summer.png",scale=2,dpi=300, units="cm",width=30,height=7)

#Fall, all zoop BPUE (mg): facets of stacked line plots
(bfp<-ggplot(bf, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(bf$bpue)/1000)
                       ))

#ggsave(bfp, file="zoop_bpue_all_fall.png",scale=2,dpi=300, units="cm",width=30,height=7)

#now, for fall, make a plot for each region separately
#do this so you can patch them together for report
bfdt<-subset(bf,region=="dt")
bfss<-subset(bf,region=="ss")
bfspl<-subset(bf,region=="spl")

#delta plot
(p_bz_d <- ggplot(bfdt, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    theme(legend.position="none") + 
    scale_x_continuous("Year (September - November)", limits=c(1966,2018)) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       , limits=c(0,38) #set the max by hand based on all three regional plots for the season
                       
    ))

#then Suisun plot
(p_bz_ss <- ggplot(bfss, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    theme(legend.position="none") + 
    scale_x_continuous("Year (September - November)", limits=c(1966,2018)) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       , limits=c(0,38) #set the max by hand based on all three regional plots for the season
                       
    ))

#then San Pablo plot
(p_bz_sp <- ggplot(bfspl, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+theme(legend.position=c(0.2,0.7)) +
    scale_x_continuous("Year (September - November)", limits=c(1966,2018)) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       , limits=c(0,38) #set the max by hand based on all three regional plots for the season
                       
    ))



#BPUE, mysids excluded, each season is separate panel-----------------

bwnm<-subset(zmeans,quarter=="Q1" & taxon!="mys")
bspnm<-subset(zmeans,quarter=="Q2" & taxon!="mys")
bsunm<-subset(zmeans,quarter=="Q3" & taxon!="mys")
bfnm<-subset(zmeans,quarter=="Q4" & taxon!="mys")


#Winter, all zoop except mysids BPUE (mg): facets of stacked line plots
(bwnmp<-ggplot(bwnm, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(bwnm$bpue/1000))  #this seems to mess up line for calanoids
    ))

#ggsave(bwnmp, file="zoop_bpue_no_mysids_winter.png",scale=2,dpi=300, units="cm",width=30,height=7)


#Spring, all zoop except mysids BPUE (mg): facets of stacked line plots
(bspnmp<-ggplot(bspnm, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(bspnm$bpue)/1000)
    ))

#ggsave(bspnmp, file="zoop_bpue_no_mysids_spring.png",scale=2,dpi=300, units="cm",width=30,height=7)


#Summer, all zoop except mysids BPUE (mg): facets of stacked line plots
(bsunmp<-ggplot(bsunm, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(bsunm$bpue)/1000)
    ))

#ggsave(bsunmp, file="zoop_bpue_no_mysids_summer.png",scale=2,dpi=300, units="cm",width=30,height=7)

#Fall, all zoop except mysids BPUE (mg): facets of stacked line plots
(bfnmp<-ggplot(bfnm, aes(x = qyear, y = bpue/1000, fill = taxon)) + 
    geom_area(position = 'stack')+
    theme_iep()+
    #theme(legend.position="none") + 
    scale_x_continuous("Year", limits=c(1966,2018)) +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C / m"^" 3", ")"))
                       #, limits=c(0,max(bfnm$bpue)/1000)
    ))

#ggsave(bfnmp, file="zoop_bpue_no_mysids_fall.png",scale=2,dpi=300, units="cm",width=30,height=7)



###Chlorophyll-a: code to create plots and then combine them with the zooplankton plots------------

str(chlora)

#create subsets for each quarter
fall<-subset(chlora,quarter=="Q4")
winter<-subset(chlora,quarter=="Q1")
spring<-subset(chlora,quarter=="Q2")
summer<-subset(chlora,quarter=="Q3")

#set order of quarters and regions for plotting
chlora$quarter = factor(chlora$quarter, levels=c('Q1','Q2','Q3','Q4'))
chlora$region = factor(chlora$region, levels=c('spl','ss','dt'))


#now, for fall, make a plot for each region separately
#do this so you can patch them together for report
fdt<-subset(fall,region=="dt")
fss<-subset(fall,region=="ss")
fspl<-subset(fall,region=="spl")

#delta plot
(p_chla_d <- ggplot(fdt, aes(x=qyear, y=chla))+
    geom_vline(xintercept = 1986, size=1,linetype = "longdash",color="dark gray")+ #clam invasion
    annotate("text", label = "clam invasion", x = 1994, y = 20.12, color = "black", size=5)+
    geom_line(colour="black", size=1.3)+geom_point(colour="black",size=3) +
    theme_iep() +
    theme(legend.position="none") + 
    scale_y_continuous(expression(paste("Chlorophyll-a (",mu,"g / L)")),limits=c(0, max(fall$chla))) +
    scale_x_continuous("", limits=c(1966,2018)) )

#scale_y_continuous(expression(paste("Zooplankton Biomass (",mu,"g C/m"^" 3", ")")), limits=c(0,max(zmeans$bpue))))


#then Suisun plot
(p_chla_ss <- ggplot(fss, aes(x=qyear, y=chla))+
    geom_vline(xintercept = 1986, size=1,linetype = "longdash",color="dark gray")+ #clam invasion
    annotate("text", label = "clam invasion", x = 1994, y = 20.12, color = "black", size=5)+
    geom_line(colour="black", size=1.3)+geom_point(colour="black",size=3) +
    theme_iep() +
    theme(legend.position="none") + 
    scale_y_continuous(expression(paste("Chlorophyll-a (",mu,"g / L)")),limits=c(0, max(fall$chla))) +
    scale_x_continuous("", limits=c(1966,2018)) )

#then San Pablo plot
(p_chla_sp <- ggplot(fspl, aes(x=qyear, y=chla))+
    geom_vline(xintercept = 1986, size=1,linetype = "longdash",color="dark gray")+ #clam invasion
    annotate("text", label = "clam invasion", x = 1994, y = 20.12, color = "black", size=5)+
    geom_line(colour="black", size=1.3)+geom_point(colour="black",size=3) +
    theme_iep() +
    theme(legend.position="none") + 
    scale_y_continuous(expression(paste("Chlorophyll-a (",mu,"g / L)")),limits=c(0, max(fall$chla))) +
    scale_x_continuous("", limits=c(1966,2018)) )

#creates a grid of plots where everything lines up properly; chlor-a and zoop
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

pchz<-plot_grid(p_chla_sp,p_chla_ss,p_chla_d,p_bz_sp,p_bz_ss,p_bz_d, ncol = 3, nrow = 2, align="v")
pchz #eventualy the plot will render but it's slow

#ggsave(pchz, file="zoop+chla_fall_panel.png", path=plot_folder,scale=1.8, dpi=300, units="cm",width=25.5,height=10.8)












