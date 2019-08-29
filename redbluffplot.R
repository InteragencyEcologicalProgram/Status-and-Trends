#Graph the red bluff data

library(tidyverse)
library(lubridate)

#data from: http://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_daily.php?outputFormat=csv&year=2019&biweekly=other&wtemp=default

rb = function(nfiles) {
  redbluff = read.csv("redbluffdaily_1.csv",  stringsAsFactors =F)
  redbluff = redbluff[1:(which(redbluff$Project == "Notes:")-1),]
   
   for (i in 2:nfiles) {
  path = paste("redbluffdaily_", i, ".csv", sep = "")
  rb2 = read.csv(path,  stringsAsFactors =F)
  rb2 = rb2[1:(which(rb2$Project == "Notes:")-1),]
  redbluff = rbind(redbluff, rb2)
   }
  return(redbluff)
}

redbluff = rb(16)

redbluff$Date = as.Date(redbluff$Date, format = "%Y-%m-%d")

#subset days sampling occured
Redbluff2 = redbluff[which(redbluff$Winter.Chinook.BY != "--"), ]

#Drop the columns we’re nto interested in
Redbluff3 = Redbluff2[,c(2,5,8,11, 14)]

#make it long instead of wide
Redlong = gather(Redbluff3, key = "runname", value = "Dailypassage", -Date) 

#passage estimates as a numeric
Redlong$Dailypassage = as.numeric(Redlong$Dailypassage)

#calculate the average
MeanPass = group_by(Redlong, year(Date), runname) %>% summarize(meandaily = mean(Dailypassage, na.rm = T))

#average passage just for winter
Redlong$Month = month(Redlong$Date)
RedWinter = filter(Redlong, Month == 12 | Month == 1 | Month == 2)

#lump december with jan and feb
RedWinter$Year2 = year(RedWinter$Date)
RedWinter$Year2[which(RedWinter$Month== 1 | RedWinter$Month == 2)] = year(RedWinter$Date[which(RedWinter$Month== 1 | RedWinter$Month == 2)])-1

#calculate mean passage
meanwinter = group_by(RedWinter, Year2, runname) %>% summarize(meandaily = mean(Dailypassage, na.rm = T))


#quick plot
Red = ggplot(meanwinter, aes(x = Year2, y = meandaily, fill = runname))
Red + geom_bar(stat = "identity")




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
          plot.margin = unit(c(0.25, 0.4, 0.1, 0.4), units = , "cm"), #adjusted the "top" and "right" values so nothing is cut off
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 9, face = "plain"),
          legend.title=element_text(size=10))
}

#Fall Chinook dominate the graph so much it is hard to see the other things, so I won't worry abou tit.
Red = ggplot(meanwinter, aes(x = Year2, y = meandaily))
Red + geom_bar(stat = "identity") + ylab("Estimated dialy passage") + xlab("Year (December-January)") +
  theme_iep()
