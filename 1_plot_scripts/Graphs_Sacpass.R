#plot spring-run adult salmon

library(lubridate)

#source(file.path(data_access_root,"data_access_sacpass.R"))

load(file.path(data_root, "Grantab.RData"))

#Spring run salmon

SpringRun_1966 <- ggplot(Spring, aes(x=Year, y=sprinrun))+
    geom_bar(stat="identity") +
    smr_theme_update() +
    theme(legend.position="none") + 
		ylab("Adult Spring-Run returns") + 
    smr_x_axis(report_year, type = "all", season = "spring") +
		stat_missing(aes(x=Year, y=sprinrun), size=2.5) + 
		stat_lt_avg(aes(y = sprinrun)) + 
		smr_caption(stat_name="adult Spring-Run Chinook Salmon return", 
		            report_year=report_year) + 
		smr_alttext(stat_name="adult Spring-Run Chinook Salmon returns")

SpringRun_1966

getCaption(SpringRun_1966)
getAlttext(SpringRun_1966)

save(list="SpringRun_1966", file=file.path(fig_root_spring,"SpringRun_1966.RData"))


#Fall run salmon
FallRun_1966 <- ggplot(Fall, aes(x=Year, y=fallrun))+
  geom_bar(stat="identity") +
  smr_theme_update() +
  theme(legend.position="none") + 
	ylab("Adult Fall-Run Returns") + 
  smr_x_axis(report_year, type = "all", season = "fall") +
	stat_missing(aes(x=Year, y=fallrun), size=2.5) + 
  stat_lt_avg(aes(y = fallrun)) + 
	smr_caption(stat_name="adult Fall-Run Chinook Salmon return", report_year=report_year) + 
	smr_alttext(stat_name="adult Fall-Run Chinook Salmon returns")

FallRun_1966

getCaption(FallRun_1966)
getAlttext(FallRun_1966)

save(list="FallRun_1966", file=file.path(fig_root_fall,"FallRun_1966.RData"))


#Red Bluff

#redbluff = read.csv(file.path(data_root,"redbluff_all.csv"))
redbluff = Redlong

#calculate the average
MeanPass = group_by(redbluff, lubridate::year(Date), runname) %>% summarize(meandaily = mean(Dailypassage, na.rm = T))

#average passage just for winter
redbluff$Month = lubridate::month(redbluff$Date)
RedWinter = filter(redbluff, Month == 12 | Month == 1 | Month == 2)

#lump december with jan and feb
RedWinter$Year2 = lubridate::year(RedWinter$Date)
RedWinter$Year2[which(RedWinter$Month== 1 | RedWinter$Month == 2)] = lubridate::year(RedWinter$Date[which(RedWinter$Month== 1 | RedWinter$Month == 2)])-1

#calculate mean passage
meanwinter = group_by(RedWinter, Year2, runname) %>% summarize(meandaily = mean(Dailypassage, na.rm = T))


#We just wanted winter run for the winter report
Red = ggplot(filter(meanwinter, runname == "Winter.Chinook.Passage.Estimate"), 
							aes(x = Year2, y = meandaily))

redbluff_2003 = Red + geom_bar(stat = "identity") + 
 #smr_y_axis(name = "Estimated daily passage") + 
 ylab("Estimated daily passage") + 
 smr_x_axis(report_year, type = "recent", season = "winter")+
  smr_theme_update()+
	stat_missing(aes(x=Year2, y=meandaily), size=2.5) + 
  stat_lt_avg(aes(y = meandaily)) + 
	smr_caption(stat_name="the adult Fall-Run Chinook population estimate", 
							report_year=report_year) + 
	smr_alttext(stat_name="adult Fall-Run Chinook population estimates")

redbluff_2003

getCaption(redbluff_2003)
getAlttext(redbluff_2003)

save(list="redbluff_2003", file=file.path(fig_root_winter,"redbluff_2003.RData"))

