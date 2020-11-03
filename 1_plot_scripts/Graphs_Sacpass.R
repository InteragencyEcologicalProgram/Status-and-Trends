#plot spring-run adult salmon

source(file.path(data_access_root,"data_access_sacpass.R"))


#Spring run salmon

p_spch <- ggplot(Spring, aes(x=Year, y=sprinrun))+
    geom_bar(stat="identity") +
    smr_theme() +
    theme(legend.position="none") + 
    smr_y_axis(name = "Spring Run Chinook Adult Returns") +
    smr_x_axis(report_year, type = "all", season = "spring") +
  stat_lt_avg(aes(y = sprinrun))
p_spch

ggsave(p_spch, file="SpringRun_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_spring)


#Fall run salmon
p_frch <- ggplot(Fall, aes(x=Year, y=FallRun))+
  geom_bar(stat="identity") +
  smr_theme() +
  theme(legend.position="none") + 
  smr_y_axis(name = "Fall Run Chinook Adult Returns") +
  smr_x_axis(report_year, type = "all", season = "fall") +
  stat_lt_avg(aes(y = FallRun))

p_frch

ggsave(p_frch, file="FallRun_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)


#Red Bluff

redbluff = read.csv(file.path(data_root,"redbluff_all.csv"))

redbluff$Date = as.Date(redbluff$Date, format = "%Y-%m-%d")

#calculate the average
MeanPass = group_by(redbluff, year(Date), runname) %>% summarize(meandaily = mean(Dailypassage, na.rm = T))

#average passage just for winter
redbluff$Month = month(redbluff$Date)
RedWinter = filter(redbluff, Month == 12 | Month == 1 | Month == 2)

#lump december with jan and feb
RedWinter$Year2 = year(RedWinter$Date)
RedWinter$Year2[which(RedWinter$Month== 1 | RedWinter$Month == 2)] = year(RedWinter$Date[which(RedWinter$Month== 1 | RedWinter$Month == 2)])-1

#calculate mean passage
meanwinter = group_by(RedWinter, Year2, runname) %>% summarize(meandaily = mean(Dailypassage, na.rm = T))


#We just wanted winter run for the winter report
Red = ggplot(filter(meanwinter, runname == "Winter.Chinook.Passage.Estimate"), aes(x = Year2, y = meandaily))
redtest = Red + geom_bar(stat = "identity") + 
 smr_y_axis(name = "Estimated daily passage") + 
 smr_x_axis(report_year, type = "recent", season = "winter")+
  smr_theme()+
  stat_lt_avg(aes(y = meandaily))
redtest

#save it in the right place
ggsave(redtest, file="redbluff_2003.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_winter)
# ggsave(redtest2, file="redbluff_2003.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")

