#plot spring-run adult salmon

source(file.path(data_access_root,"data_access_sacpass.R"))


#Spring run salmon
Spring$fyear = as.factor(Spring$Year)
p_spch <- ggplot(filter(Spring, Year <= 2018), aes(x=fyear, y=sprinrun))+
    geom_bar(stat="identity") +
    theme_smr() +
    theme(legend.position="none") + 
    scale_y_continuous("Spring Run Chinook Adult Returns",limits=c(0, max(Spring$sprinrun))) +
    std_x_axis_all_years(2018, "discrete") +
  std_x_axis_label("spring")+
  lt_avg_line(mean(Spring$sprinrun))
p_spch

ggsave(p_spch, file="SpringRun_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_spring)


#Fall run salmon
Fall$fyear = as.factor(Fall$Year)
p_frch <- ggplot(Fall, aes(x=fyear, y=FallRun))+
  geom_bar(stat="identity") +
  theme_smr() +
  theme(legend.position="none") + 
  scale_y_continuous("Fall Run Chinook Adult Returns",limits=c(0, max(Fall$FallRun))) +
  std_x_axis_all_years(2018, "discrete") +
  std_x_axis_label("fall")+
  lt_avg_line(mean(Fall$FallRun))
p_frch

ggsave(p_frch, file="FallRun_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_fall)


#Red Bluff

redbluff = read.csv(file.path(data_root,"redbluff_all.csv"))
str(redbluff)

redbluff$Date = as.Date(redbluff$Date, format = "%Y-%m-%d")

#calculate the average
MeanPass = group_by(redbluff, year(Date), runname) %>% summarize(meandaily = mean(Dailypassage, na.rm = T))

#average passage just for winter
redbluff$Month = month(redbluff$Date)
RedWinter = filter(redbluff, Month == 12 | Month == 1 | Month == 2)

#lump december with jan and feb
RedWinter$Year2 = year(RedWinter$Date)
RedWinter$Year2[which(RedWinter$Month== 1 | RedWinter$Month == 2)] = year(RedWinter$Date[which(RedWinter$Month== 1 | RedWinter$Month == 2)])-1

RedWinter = filter(RedWinter, Year2 < 2018)

#calculate mean passage
meanwinter = group_by(RedWinter, Year2, runname) %>% summarize(meandaily = mean(Dailypassage, na.rm = T))


#We just wanted winter run for the winter report
Red = ggplot(filter(meanwinter, runname == "Winter.Chinook.Passage.Estimate"), aes(x = Year2, y = meandaily))
redtest = Red + geom_bar(stat = "identity") + ylab("Estimated daily passage") + xlab("Year (December-January)") +
  coord_cartesian(ylim = c(0, 4300))+
  std_x_axis_all_years(2018)+
  theme_smr() + annotate("text", label = ("Data were not collected until 2004"), x = 1980, y = 100, size = 2.5)

redtest

ggsave(redtest, file="redbluff_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_winter)

#Calculate average
MeanAll = mean(filter(meanwinter, runname == "Winter.Chinook.Passage.Estimate")$meandaily)


#recent trends
meanwinter$fyear = as.factor(meanwinter$Year2)
Red = ggplot(filter(meanwinter, runname == "Winter.Chinook.Passage.Estimate"), aes(x = fyear, y = meandaily))
redtest2 = Red + geom_bar(stat = "identity") + 
  ylab("Estimated daily passage") + 
  xlab("Year (December-January)") +
  coord_cartesian(ylim = c(0, 4300))+
  std_x_axis_rec_years(2018)+
  std_x_axis_label("winter")+
  geom_hline(aes(yintercept = MeanAll), linetype  = "dashed", col = "red", size = 0.9) +
  theme_smr()

redtest2

#save it in the right place
ggsave(redtest2, file="redbluff_2003.png", dpi=300, units="cm", width=9.3, height=6.8, path = fig_root_winter)
# ggsave(redtest2, file="redbluff_2003.png", dpi=300, units="cm", width=9.3, height=6.8, path = "report_bookdown/figures")

