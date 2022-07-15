#IEP Status and Trends Report
#all seasons
#Data up to and including 2018
#Zooplankton Biomass Per Unit Effort

#Created by Nick Rasmussen
#librally updated by Rosemary Hartman
#last updated: 11/4/2020

#Clark-Bumpus Net: Calanoid copepods, Cladocerans, Cyclopoids (Acanthocyclops and other cyclopoids)
#Rotifer pump: Cyclopoids (Oithona, Limnoithona)
#Mysid net: all species

source("setup.R")

#packages

#Install the package we wrote for this project (only needs to be done once)
#devtools::install_github("InteragencyEcologicalProgram/smonitr", "v1.2.0.9000")

#load all the packages you need
library(colorspace) #color palette
library(zoo) ## yearmon and yearqtr classes
library(cowplot)
library(lubridate)
library(readxl)
library(tidyverse)
library(smonitr)

#import datasets----------------
#source("0_data_access_scripts/data_access_zoops.R")
#source("1_plot_scripts/mysid biomass.R")
source("1_plot_scripts/legends.R")


#details of sampling station locations
station<-read.csv(file.path(data_root,"zoop_stations.csv")) 

#carbon mass for individual zooplankton taxa (cladocerans, copepods)
zmass<-read.csv(file.path(data_root,"zoop_individual_mass.csv")) 
load(file.path(data_root, "Zoops.RData"))
#load(file.path(data_root, "mysidbiomass.RData"))


#CB net: formatting data---------
#reduce count and biomass data to just needed elements and then use to calculate biomass


#exclude unneeded rows:
#exclude EZ stations (NZEZ2, NZEZ6)
#exclude non-core stations (core = 0); 1 = sampled since 1972, 2 = sampled since 1974
#exclude 1972-1973 because only subset of core stations were sampled during those years
#exclude survey replicate 2 for the months that have those; vast majority of monthly surveys are just one replicate
#exclude the winter months (1,2,12) initially because they have a shorter time series than the other months
zoop_sub<-subset(zoopscb, Station!="NZEZ2" & Station!="NZEZ6" & Core!=0 & 
                   Year>1973 & SurveyRep!=2 & Survey>2 & Survey<12)

#create separate subset of the two san pablo stations to include in graphs (no core stations in SP)
zoop_subsp<-subset(zoopscb, Station=="NZD41" | Station=="NZ41A")
unique(zoop_subsp$SurveyRep) #only includes one sample per month
range(zoop_subsp$Year) #starts after 1998

#create separate subset for winter months with its shorter time series
zoop_subw<-subset(zoopscb, Station!="NZEZ2" & Station!="NZEZ6" & Core!=0 & Year>1993 & 
                    SurveyRep!=2 & (Survey<3 | Survey>11))

unique(zoop_subw$Station)

#combine the three data subsets
zoop_s<-rbind(zoop_sub,zoop_subsp,zoop_subw)



#reduce zooplankton count data to just needed columns. Take out the cyclopoids best sampled
#by the pump
zoop<-subset(zoop_s,select=c("Year","Survey","SampleDate","Station","ACARTELA","ACARTIA","DIAPTOM","EURYTEM","OTHCALAD","PDIAPFOR",
                             "PDIAPMAR","SINOCAL","TORTANUS",
                             "ACANTHO","OTHCYCAD","BOSMINA","DAPHNIA","DIAPHAN","OTHCLADO")  )


#convert zoop data frame from wide to long format
zoopl<-pivot_longer(zoop, cols = (ACARTELA:OTHCLADO), names_to= "taxon", values_to = "cpue")

#modify column headers
#names(zoopl)[1:4]<-c("year","survey","date","station")

#format some columns
zoopl$taxon<-factor(zoopl$taxon)

#add in individual biomass data
zoopb<-left_join(zoopl,zmass,by="taxon")

#calculate biomass by sample-taxon combo
zoopb$bpue<-zoopb$cpue*zoopb$mass_indiv_ug


#pump: formatting data---------___________________________________

names(zoopps)
#zoopp = rename(zoopp, Station = StationNZ)

#exclude unneeded rows:
#exclude EZ stations (NZEZ2, NZEZ6)
#exclude non-core stations (core = 0); 1 = sampled since 1972, 2 = sampled since 1974
#exclude 1972-1973 because only subset of core stations were sampled during those years
#exclude survey replicate 2 for the months that have those; vast majority of monthly surveys are just one replicate
#exclude the winter months (1,2,12) initially because they have a shorter time series than the other months
zoop_sub2<-subset(zoopps, Station!="NZEZ2" & Station!="NZEZ6" & Core!=0 & Year>1973 & 
                    SurveyRep!=2 & Survey>2 & Survey<12)

#create separate subset of the two san pablo stations to include in graphs (no core stations in SP)
zoop_subsp2<-subset(zoopps, Station=="NZD41" | Station=="NZ41A")

#create separate subset for winter months with its shorter time series
zoop_subw2<-subset(zoopps, Station!="NZEZ2" & Station!="NZEZ6" & Core!=0 & Year>1993 & 
                     SurveyRep!=2 & (Survey<3 | Survey>11))

#combine the three data subsets
zoop_s2<-rbind(zoop_sub2,zoop_subsp2,zoop_subw2)

#reduce zooplankton data to just needed columns
#"Year","Survey","SampleDate","Station" 
#"LIMNOSINE"+"LIMNOSPP"+"LIMNOTET" + "OITHDAV" + "OITHSIM" + "OITHSPP" = the two cyclopoid genera best surveyed by pump
zoop2<-subset(zoop_s2,select=c("Year","Survey","SampleDate","Station","LIMNOSINE",
                               "LIMNOSPP","LIMNOTET","OITHDAV","OITHSIM","OITHSPP"))

#convert zoop data frame from wide to long format
zoopl2<-gather(zoop2,taxon,cpue,LIMNOSINE:OITHSPP)

#name columns
#names(zoopl2)[1:4]<-c("year","survey","date","station")

#add in individual biomass data
zoopb2<-left_join(zoopl2,zmass,by="taxon",type="left")

#calculate biomass by sample-taxon combo
zoopb2$bpue<-zoopb2$cpue*zoopb2$mass_indiv_ug

#combine the CB and pump data sets
names(zoopb)
names(zoopb2)

zall<-rbind(zoopb,zoopb2)
######################################################################################

#first, create vectors of strings to replace with broader category names 
calan<-c("ACARTELA","ACARTIA","DIAPTOM","EURYTEM","OTHCALAD","PDIAPFOR","PDIAPMAR","SINOCAL","TORTANUS")
cyclop<-c("ACANTHO","OTHCYCAD","LIMNOSINE","LIMNOSPP","LIMNOTET" , "OITHDAV" , "OITHSIM" , "OITHSPP")
cladoc<-c("BOSMINA","DAPHNIA","DIAPHAN","OTHCLADO")


Zoopcats = data.frame(taxon = c(calan, cyclop, cladoc), 
                      cat = c(rep("cala", length(calan)), 
                              rep("cycl", length(cyclop)), 
                              rep("clad", length(cladoc))) )


zall2 = merge(zall, Zoopcats) 


#add together biomass for all species within each of the three taxonomic groups
zll<- group_by(zall2, Year, Survey, SampleDate, Station, cat) %>%
  summarize(cpue = sum(cpue, na.rm = T), bpue = sum(bpue, na.rm = T))




#mysids: formatting data---------

#next format the bpue dataset
#we just want total mysid BPUE
MysidBPUEx = MysidBPUE %>%
  group_by(SurveyCode, Year, Survey, SurveyRep, SampleDate, StationNZ, Time) %>%
summarize(bpue = sum(`Acanthomysis aspera`, `Acanthomysis hwanhaiensis`, 
                     `Alienacanthomysis macropsis`, `Deltamysis holmquistae`,
                                         `Hyperacanthomysis longirostris`, `Neomysis kadiakensis`, 
                                         `Neomysis mercedis`,  Unidentified)) %>%
  rename(Station = StationNZ) %>%
  mutate(taxon = "mys")


#create column with bpue in micrograms instead of milligrams because rest of zoop is in micrograms
MysidBPUEx$bpue<-MysidBPUEx$bpue*1000

#combine all zoop data sets
zll = mutate(zll, taxon = cat, cat = NULL)
mza<-bind_rows(zll, MysidBPUEx)

#Stations: formatting data---------

names(station)

#combine deg, min, zoop for lat and for long

#first calculate decimal deg, min, sec
station$long_dec<-station$long_deg + station$long_min/60 + station$long_sec/3600

#reduce to just needed columns
stati<-subset(station,select=c("station","long_dec")) %>%
  rename(Station = station)
str(stati)
#sort by longitude
stati<-stati[order(stati$long_dec),]

#merge with zooplankton cpue data
mcpg<-left_join(mza, stati)

unique(mcpg$Station) #18 stations: 16 core stations + 2 san pablo stations

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
  mutate( month = month(SampleDate), 
              ym =as.yearmon(paste(month, Year), format =  "%m %Y"),
              yq =as.yearqtr(ym + 1/12, format = "%Y Q%q"),
              yq2 = yq) %>%
  separate(yq2, c('qyear', 'quarter'), sep=" ") %>%
  mutate(quarter = factor(quarter, levels=c('Q1','Q2','Q3','Q4')),
         qyear = as.integer(qyear)) %>%
  filter(!is.na(region), !(Year < 1975), !(Year<1998 & region == "spl"))


#generate means by region, year, quarter, and taxon
#zmeans<-aggregate(cbind(cpue,bpue)~region+qyear+quarter+taxon,data=mcpg,FUN=mean,na.rm=T)

zmeans = group_by(mcpg, region, qyear, quarter, taxon) %>%
  summarise(cpue = mean(cpue), bpue = mean(bpue))
str(zmeans)
zmeans = mutate(zmeans, bpue_mg = bpue/1000 )

#set up facet labels
season_names<-c('Q1'="winter",'Q2'="spring",'Q3'="summer",'Q4'="fall")
region_names<-c('dt'="Delta",'ss'="Suisun",'spl'="San Pablo")

#custom colors
(custcol<-diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))
#"#664F2B" "#928678" "#748D83" "#095E49"


#BPUE plots: stacked line plots for each season and region------------



#All zoop BPUE (ug): facets of stacked line plots
bpl<-ggplot(zmeans, aes(x = qyear, y = bpue, fill = taxon)) + 
    geom_area(position = 'stack')+
    smr_theme()+
    #theme(legend.position="none") + 
   smr_x_axis(report_year, type = "all", season = "spring") +
    facet_grid(quarter~region
               ,labeller = as_labeller(
                 c(region_names,season_names))) +
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))#+
#    scale_y_continuous(expression(paste("Zooplankton Biomass (",mu,"g C/m"^" 3", ")")), limits=c(0,max(zmeans$bpue))))
#NOTE: mysids overwhelm everything else because they are so much larger

bpl


zoops = function(reg, quart, data, reportyear, verbose=TRUE) {
  dat = filter(data, quarter == quart, region == reg)
  
  #calculate long-term average
  sums = group_by(dat, qyear) %>% summarize(bpuetot = sum(bpue_mg))
  meanB = mean(sums$bpuetot)

  #make a plot
  bpl <- ggplot(dat, aes(x = qyear, y = bpue_mg, fill = taxon)) + 
    geom_area(position = 'stack') +
		smr_theme_update() + 
    scale_x_continuous("Year", limits=c(1966,report_year)) + 
		
		# Include for caption automation functions:
		stat_missing(aes(x=qyear, y=bpue_mg), size=NA) +
    
    #################NOTE: I can't figure out how to put a missing value triangle for 2021 without
    #having them show up for pre 1975 too, so i'm hard coding this in for now. Lara can probably do better.
    annotate("point", x = 2021, y = 0, shape = 24, size = 4, fill = "tan2", color = "gray10")+
		
    #add long-term average line:
    geom_hline(aes(yintercept=meanB), size=0.9, color="red", linetype="dashed") + 
		
    #format the colors and legend:
    scale_fill_manual(name=NULL, labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids"),
                      values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7),
                      guide=guide_legend(keyheight=0.5, title=NULL, 
																				 direction="horizontal", 
                                         label.position="right"#, 
																				 #label.theme=element_text(size=4)
																				 )) + 
    theme(legend.position=c(0.5, 0.85), 
					#legend.spacing.x=unit(0.1, 'cm'),  
					legend.key.size=unit(0.4,"cm"), 
					legend.box.spacing=unit(0, units="cm"),
					legend.background=element_blank()) + 
		guides(fill=guide_legend(nrow=2, byrow=TRUE)) + 																				 
    # scale_y_continuous(expression(paste("Zooplankton Biomass (mg C/m"^"3",")")), 
    scale_y_continuous(expression(paste("Biomass (mg C/m"^"3",")")), 
                       limits=c(0,max(zmeans$bpue_mg)))

  # teh san pablo bay graph needs annotation for date of first collection,
  #and it will be the one with the legend on top
  if(reg == "spl") {
    bpl = bpl + 
			annotate("text", x = 1966, y = 20, 
							 label = "Data were not \n collected until 1998", hjust="left", size=2.7)
	
  } else if(quart == "Q1") {
    
    #winter graphs also need annotation for date of first complete collection
    bpl = bpl +
			theme(legend.position = "none") +
      annotate("text", x = 1966, y = 20,
               label = "Data were consistently \n collected starting in 1995",
               hjust = "left", size=2.7)

  } else {
    bpl = bpl + 
			theme(legend.position = "none")

  }
	
	## Caption and alttext need to be the second to last and last layers:
	regPretty <- sub("Delta","the Delta",region_names[reg])
	quartPretty <- season_names[quart]
	
	fmt_caption <- "total zooplankton biomass in %s"
	statNameCaption <- sprintf(fmt=fmt_caption, regPretty)

	fmt_alttext <- "total %s zooplankton biomass in %s"
	statNameAlttext <- sprintf(fmt=fmt_alttext, quartPretty, regPretty)
	
  bpl <- bpl + 
		smr_caption(data=sums, aes(x=qyear, y=bpuetot, fill=NULL), 
								stat_name=statNameCaption, 
								report_year=reportyear)	+ 
		smr_alttext(data=sums, aes(x=qyear, y=bpuetot, fill=NULL), 
								stat_name=statNameAlttext)

	if(verbose) {
		print(getCaption(bpl))
		print(getAlttext(bpl))
	}
	
	return(bpl)
}

#filter to the report year 
zmeans = filter(zmeans, qyear <= report_year)


#winter zoops plot
zoops("spl", "Q1", zmeans, reportyear=report_year)
# Suisun and Delta not consistently sampled until 1995 (per email with Rosie and previous
# versions of this code):
zoops("ss", "Q1", subset(zmeans, qyear >= 1995), reportyear=report_year)
zoops("dt", "Q1", subset(zmeans, qyear >= 1995), reportyear=report_year)

#spring zoops plot

zoops("spl", "Q2", zmeans, reportyear=report_year)
zoops("ss", "Q2", zmeans, reportyear=report_year)
zoops("dt", "Q2", zmeans, reportyear=report_year)


#summer zoops plot

zoops("spl", "Q3", zmeans, reportyear=report_year)
zoops("ss", "Q3", zmeans, reportyear=report_year)
zoops("dt", "Q3", zmeans, reportyear=report_year)

#fall zoopsplot
zoops("spl", "Q4", zmeans, reportyear=report_year)
zoops("ss", "Q4", zmeans, reportyear=report_year)
zoops("dt", "Q4", zmeans, reportyear=report_year)



## Save all region and season combinations:
for(Q in c("Q1","Q2","Q3","Q4")) {
	zoopPlotList <- list()

	for(R in c("dt","spl","ss")) {
	  tmp <- paste("zoop", R, sep="_")
	  use_data <- zmeans
	  
		if(Q == "Q1" && R %in% c("ss","dt")) {
  		# Suisun and Delta not consistently sampled during winter until 1995 (per email 
		  # with Rosie and previous versions of this code):
  		use_data <- subset(zmeans, qyear >= 1995) 		
		}
	  zoopPlotList[[tmp]] <- zoops(reg=R, quart=Q, data=use_data, 
	                               reportyear=report_year, verbose=FALSE)
	}
	
	varName <- paste0("zooplankton_",season_names[Q])
	fileName <- file.path(fig_root, season_names[Q], paste0(varName,".RData"))

	assign(x=varName, value=zoopPlotList)
	save(list=varName, file=fileName)
	print(sprintf("Saving plots in a list called %s in the file %s", varName, fileName))
	cat("\n")
}




#############################################################################

#Let's try one that puts all the regions together

zoops2 = function(quart, data) {
  dat = filter(data, quarter == quart)
  
  #calculate long-term average
  sums = group_by(dat, region, qyear) %>% summarize(bpuetot = sum(bpue_mg))
  meanB = group_by(sums, region) %>% summarize(bpueM =mean(bpuetot))
  
  #make a plot
  bpl<-ggplot(dat, aes(x = qyear, y = bpue_mg, fill = taxon)) + 
    geom_area(position = 'stack')+
    smr_theme()+ facet_wrap(~region) +
    theme(legend.position="top", legend.box.spacing = unit(0, units = "cm"), 
          strip.background = element_blank(),
          strip.text = element_blank()) + 
    smr_x_axis(report_year, type  = "all", season = season_names[quart])  +
    geom_hline(data = meanB, aes(yintercept = bpueM), size = 0.9, color = "red", linetype = "dashed")+
    scale_fill_manual(name = "Taxon",labels=c("Calanoids","Cladocerans","Cyclopoids","Mysids")
                      ,values=diverge_hcl(4,h=c(55,160),c=30,l=c(35,75),power=0.7))+
    scale_y_continuous(expression(paste("Zooplankton Biomass (mg C/m"^" 3", ")")), 
                       limits=c(0,max(zmeans$bpue_mg)))
  bpl
  

  
  
}

#filter to the report year and convert biomass to mg
zmeans = filter(zmeans, qyear <= report_year)
  


# Create dataframes for text comments on plots
nodata <- tibble(
  Year = 1966,
  yValue = 10,
  label = c("Data were not collected until 1998","",""),
  region = factor(c("spl", "ss","dt"))
)

summer = zoops2("Q3", zmeans) + 
  geom_text(data = nodata,
  aes(x = Year, y = yValue, label = label),
  inherit.aes = FALSE,
  hjust = "left",
  size = 1.9)


fall = zoops2("Q4", zmeans) + 
  geom_text(data = nodata,
            aes(x = Year, y = yValue, label = label),
            inherit.aes = FALSE,
            hjust = "left",
            size = 1.9)


winter = zoops2("Q1", zmeans) + 
  geom_text(data = nodata,
            aes(x = Year, y = yValue, label = label),
            inherit.aes = FALSE,
            hjust = "left",
            size = 1.9)

spring = zoops2("Q2", zmeans) + 
  geom_text(data = nodata,
            aes(x = Year, y = yValue, label = label),
            inherit.aes = FALSE,
            hjust = "left",
            size = 1.9)

ggsave(summer, file="zoops_panel_summer.png", 
       dpi=300, units="cm",width=27.9,height=7.5,
       path = fig_root_summer)

ggsave(spring, file="zoops_panel_spring.png", dpi=300, units="cm",width=27.9,height=7.5,
       path = fig_root_spring)

ggsave(winter, file="zoops_panel_winter.png", dpi=300, units="cm",width=27.9,height=7.5,
       path = fig_root_winter)

ggsave(fall, file="zoops_panel_fall.png", 
       dpi=300, units="cm",width=27.9,height=7.5,
       path = fig_root_fall)

