library(readxl)
library(lubridate)

Stations<-read_csv(file.path(data_root,"Master station key.csv"), col_types = "ccddc")%>%
  select(Source, Station, Latitude, Longitude)

Micro_season <- "Summer"

EMP<-read_csv(file.path(data_root,"WQ_Discrete_1975-2019.csv"))%>%
  select(Date, Station, Result, Parameter=AnalyteName)%>%
  filter(Parameter=="Microcystis" & !is.na(Result))%>%
  select(-Parameter)%>%
  rename(Microcystis=Result)%>%
  mutate(Source="EMP",
         Date=as.POSIXct(Date, format = "%m/%d/%Y"))

TNS <-read_excel(file.path(data_root,"STN Sample.xlsx"), guess_max=10000)%>%
  select(Date=SampleDate, Station=StationCode, Microcystis)%>%
  mutate(Source="TNS")%>%
  filter(!is.na(Microcystis))


Micro<-bind_rows(EMP, TNS)%>%
  mutate(Month=month(Date),
         Year=year(Date))%>%
  left_join(Stations, by=c("Source", "Station"))%>%
  filter(!is.na(Longitude))%>%
  mutate(Season=case_when(
    Month%in%c(12,1,2) ~ "Winter",
    Month%in%c(3,4,5) ~ "Spring",
    Month%in%c(6,7,8) ~ "Summer",
    Month%in%c(9,10,11) ~ "Fall"),
    Year=if_else(Month==12, Year-1, Year),
    Microcystis=as.integer(Microcystis)
  )%>%
  mutate(Region=case_when(
    Longitude < -122.216 ~ "San Pablo Bay",
    Longitude > -122.216 & Longitude < -121.829 ~ "Suisun Bay",
    Longitude > -121.829 ~ "Delta",
    TRUE ~ NA_character_
  ))%>%
  select(Month, Region, Microcystis, Year, Season)%>%
  filter(!is.na(Microcystis))%>%
  filter(Season%in%Micro_season)%>%
  droplevels()%>%
  group_by(Region, Year)%>%
  summarise(N_Microcystis=length(which(!is.na(Microcystis))), 
            Microcystis1=length(which(Microcystis==1))/N_Microcystis, 
            Microcystis2=length(which(Microcystis==2))/N_Microcystis, 
            Microcystis3=length(which(Microcystis==3))/N_Microcystis, 
            Microcystis4=length(which(Microcystis==4))/N_Microcystis, 
            Microcystis5=length(which(Microcystis==5))/N_Microcystis)%>%
  ungroup()%>%
  filter(N_Microcystis>0)%>%
  gather(key="Severity", value="Frequency", Microcystis1, Microcystis2, Microcystis3, Microcystis4, Microcystis5)%>%
  mutate(Severity=recode(Severity, "Microcystis1"="Absent", "Microcystis2"="Low", 
                         "Microcystis3"="Medium", "Microcystis4"="High", "Microcystis5"="Very high"))%>%
  mutate(Severity=factor(Severity, levels=c("Very high", "High", "Medium", "Low", "Absent")))%>%
  filter(Region%in%"Delta" & Severity!="Absent" & Year>=2004)


Microcystis_summer <- ggplot(data=Micro, aes(x=Year, y=Frequency, fill=Severity)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(type="div", palette="RdYlBu", 
                    guide=guide_legend(keyheight=0.5, title=NULL, direction="horizontal", 
                                       label.position="right", reverse=TRUE)) +
  smr_x_axis(report_year, type="recent", season="summer") +
  #scale_y_continuous(expand=expansion(0,0)) +
  ylab("Relative frequency") +
  smr_theme_update() +
  theme(#legend.position=c(0.15, 0.95), 
				#legend.text=element_text(size=8), 
				legend.key.size=unit(0.3,"cm"), 
				legend.spacing.x=unit(0.1, 'cm'),  
				legend.box.spacing=unit(0, units="cm"), 
				legend.margin=margin(t=0,r=0,b=2,l=-5, unit="pt")
			) + 
	#guides(fill=guide_legend(nrow=2,byrow=TRUE)) + 
  annotate("text", x=2004, y=0.1, label="Data not collected \n until 2007", 
					 hjust=0, size=2.7) + 
	smr_caption_manual(caption=paste("There were fewer observations of very high", 
																	 "<em>Microsystsis</em> than in previous years.")) + 
	smr_alttext_manual(alttext=paste0("Graph of microcystis abundance from ",
																	 min(Micro$Year)," to ",max(Micro$Year),"."))

Microcystis_summer

getCaption(Microcystis_summer)
getAlttext(Microcystis_summer)


save(list="Microcystis_summer", 
		 file=file.path(fig_root_summer,"Microcystis_summer.RData"))
