#Graph the red bluff data

library(tidyverse)
library(lubridate)

#data from: http://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_daily.php?outputFormat=csv&year=2019&biweekly=other&wtemp=default

source("winter_report/IEP_Status&Trends_util.R")



#create custom plot formatting function
theme_iep <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          axis.title.x = element_text(size = 10, face = "plain"),
          axis.title.y = element_text(size = 10, face = "plain"
                                      ,margin=margin(t = 0, r = 10, b = 0, l = 0)
          ),             
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          #plot.margin = unit(c(0.1, 0.3, 0.1, 0.9), units = , "cm"), #top, right, bottom, left
          #plot.margin = unit(c(0.25, 0.4, 0.1, 0.4), units = , "cm"), #adjusted the "top" and "right" values so nothing is cut off
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 9, face = "plain"),
          legend.title=element_text(size=10))
}

redbluff = read.csv("redbluff_all.csv")[,2:5]
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


#quick plot
Red = ggplot(meanwinter, aes(x = Year2, y = meandaily, fill = runname))
Red + geom_bar(stat = "identity")


#We just wanted winter run for the winter report
Red = ggplot(filter(meanwinter, runname == "Winter.Chinook.Passage.Estimate"), aes(x = Year2, y = meandaily))
redtest = Red + geom_bar(stat = "identity") + ylab("Estimated daily passage") + xlab("Year (December-January)") +
  coord_cartesian(xlim = c(1966, 2018), ylim = c(0, 4300))+
  theme_iep() + annotate("text", label = ("Data were not collected until 2004"), x = 1980, y = 100, size = 2.5)

redtest

#create custom plot formatting function
theme_iep <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          axis.title.x = element_text(size = 10, face = "plain"),
          axis.title.y = element_text(size = 10, face = "plain"
                                      ,margin=margin(t = 0, r = 10, b = 0, l = 0)
          ),             
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          #plot.margin = unit(c(0.1, 0.3, 0.1, 0.9), units = , "cm"), #top, right, bottom, left
          #plot.margin = unit(c(0.25, 0.4, 0.1, 0.4), units = , "cm"), #adjusted the "top" and "right" values so nothing is cut off
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 9, face = "plain"),
          legend.title=element_text(size=10))
}

ggsave(redtest, file="redbluff_1966.png", dpi=300, units="cm", width=9.3, height=6.8, path = "winter_report/latex/figures")

#Calculate average
MeanAll = mean(filter(meanwinter, runname == "Winter.Chinook.Passage.Estimate")$meandaily)


#recent trends
Red = ggplot(filter(meanwinter, runname == "Winter.Chinook.Passage.Estimate"), aes(x = Year2, y = meandaily))
redtest2 = Red + geom_bar(stat = "identity", fill = "tan4") + 
  ylab("Estimated daily passage") + 
  xlab("Year (December-January)") +
  coord_cartesian(xlim = c(2003, 2018), ylim = c(0, 4300))+
  scale_x_continuous(breaks = c(2005, 2010, 2015))+
  geom_hline(aes(yintercept = MeanAll), linetype  = "dashed", col = "red", size = 0.9) +
  theme_iep()

redtest2

ggsave(redtest2, file="redbluff_2003.png", dpi=300, units="cm", width=9.3, height=6.8, path = "winter_report/latex/figures")

