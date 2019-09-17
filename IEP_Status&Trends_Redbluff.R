#Graph the red bluff data

library(tidyverse)
library(lubridate)

#data from: http://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_daily.php?outputFormat=csv&year=2019&biweekly=other&wtemp=default

source("winter_report/IEP_Status&Trends_util.R")

redbluff = read.csv("redbluff_all.csv")[,2:5]
str(redbluff)

redbluff$Date = as.Date(redbluff$Date, format = "%Y-%m-%d")

#calculate the average
MeanPass = group_by(redbluff, year(Date), runname) %>% summarize(meandaily = mean(Dailypassage, na.rm = T))

#average passage just for winter
redbluff$Month = month(redbluff$Date)
RedWinter = filter(Redlong, Month == 12 | Month == 1 | Month == 2)

#lump december with jan and feb
RedWinter$Year2 = year(RedWinter$Date)
RedWinter$Year2[which(RedWinter$Month== 1 | RedWinter$Month == 2)] = year(RedWinter$Date[which(RedWinter$Month== 1 | RedWinter$Month == 2)])-1

#calculate mean passage
meanwinter = group_by(RedWinter, Year2, runname) %>% summarize(meandaily = mean(Dailypassage, na.rm = T))


#quick plot
Red = ggplot(meanwinter, aes(x = Year2, y = meandaily, fill = runname))
Red + geom_bar(stat = "identity")


#We just wanted winter run for the winter report
Red = ggplot(filter(meanwinter, runname == "Winter.Chinook.Passage.Estimate"), aes(x = Year2, y = meandaily))
redtest = Red + geom_bar(stat = "identity") + ylab("Estimated daily passage") + xlab("Year (December-January)") +
  coord_cartesian(xlim = c(1966, 2018), ylim = c(0, 4300))+
  theme_iep() + annotate("text", label = ("Data were not collected until 2004"), x = 1980, y = 100, size = 2.5)

redtest

ggsave(redtest, file="redbluff_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "winter_report/latex/figures")



#recent trends
Red = ggplot(filter(meanwinter, runname == "Winter.Chinook.Passage.Estimate"), aes(x = Year2, y = meandaily))
redtest2 = Red + geom_bar(stat = "identity") + ylab("Estimated daily passage") + xlab("Year (December-January)") +
  coord_cartesian(xlim = c(2003, 2018), ylim = c(0, 4300))+
  theme_iep()

redtest2

ggsave(redtest2, file="redbluff_2003.png", dpi=300, units="cm", width=9.3, height=6.8, path = "winter_report/latex/figures")

