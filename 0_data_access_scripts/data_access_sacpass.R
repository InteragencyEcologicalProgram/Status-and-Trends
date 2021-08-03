#download relevant datasets from sacpass

#http://www.cbr.washington.edu/sacramento/


#First I"ll look at redbluff
#unfortunately, I need to dowload it one year at a time.

library(tidyverse)
library(lubridate)
library(smonitr)



urls =  "http://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_daily.php?outputFormat=csv&year=2020&biweekly=other&wtemp=default"

#notes:
#For passage estimates values: “—“ (two dashes) indicates that no sampling occurred, 
# 0 indicates that sampling occurred but there were 0 fish.
#For length values: “—“ (two dashes) indicates that no sampling occurred, 
#NA when passage estimates is 0, otherwise a single value or a range of values with a dash.


rb = function(reportyear) {
  years = 2004:reportyear
  urls = map(years, function(x){
    paste("http://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_daily.php?outputFormat=csv&year=",x, 
          "&biweekly=other&wtemp=default", sep = "") })
  redbluff = read.csv(urls[[1]],  stringsAsFactors =F)
  redbluff = redbluff[1:(which(redbluff$Project == "Notes:")-1),]
  
  for (i in 2:length(years)) {
      rb2 = read.csv(urls[[i]],  stringsAsFactors =F)
    rb2 = rb2[1:(which(rb2$Project == "Notes:")-1),]
    redbluff = rbind(redbluff, rb2)
  }
  return(redbluff)
}

redbluff = rb(report_year)

redbluff$Date = as.Date(redbluff$Date, format = "%Y-%m-%d")

#subset days sampling occured
Redbluff2 = redbluff[which(redbluff$Winter.Chinook.BY != "--"), ]

#Drop the columns we’re not interested in
Redbluff3 = Redbluff2[,c(2,5,8,11, 14)]

#make it long instead of wide
Redlong = gather(Redbluff3, key = "runname", value = "Dailypassage", -Date) 

#passage estimates as a numeric
Redlong$Dailypassage = as.numeric(Redlong$Dailypassage)

#write the result
write.csv(Redlong, file.path(data_root,"redbluff_all.csv"), row.names = FALSE)


############################################################################################################

#Now let's look at the GrandTab adult return data

#Try out Michael's spiffy new function

Grandtab = get_grandtab_data(
  season = c("Spring", "Fall"))

Spring = Grandtab$Spring %>%
  rename(sprinrun = Annual)
Fall = Grandtab$Fall %>%
  rename(fallrun = Annual)
#well, that was spiffy and easy

Winter = get_grandtab_data(season = "Winter")
Winter = Winter[[1]]

AllAdults = left_join(Fall, Winter) %>%
  left_join(Spring)

#CSV of all the adult escapement
write.csv(AllAdults, file.path(data_root,"Grandtab_adultsalmon.csv"), row.names = F)

#save all these as a RData file
save(Spring, Winter, Fall, Redlong, file = file.path(data_root, "Grantab.RData"))
