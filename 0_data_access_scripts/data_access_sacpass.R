#download relevant datasets from sacpass

#http://www.cbr.washington.edu/sacramento/


#First I"ll look at redbluff
#unfortunately, I need to dowload it one year at a time.

source("setup.R")

library(tidyverse)
library(lubridate)
library(smonitr)



urls =  "http://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_daily.php?outputFormat=csv&year=2023&biweekly=other&wtemp=default"

#notes:
#For passage estimates values: “—“ (two dashes) indicates that no sampling occurred, 
# 0 indicates that sampling occurred but there were 0 fish.
#For length values: “—“ (two dashes) indicates that no sampling occurred, 
#NA when passage estimates is 0, otherwise a single value or a range of values with a dash.


rb = function(reportyear) {
  years = 2004:reportyear
  urls = map(years, function(x){
    paste("https://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_daily.php?sc=1&outputFormat=csv&esttype=weekly&year=",x,
          "&span=no&startdate=1%2F1&enddate=12%2F31", sep = "") })
  redbluff = read.csv(urls[[1]],  stringsAsFactors =F)
  redbluff = redbluff[1:(which(redbluff$Project == "Notes:")-1),]
  for (i in 2:length(years)) {
      rb2 = read.csv(urls[[i]],  stringsAsFactors =F)
    rb2 = rb2[1:(which(rb2$Project == "Notes:")-1),]
    redbluff = rbind(redbluff, rb2)
  }
  return(redbluff)
}

redbluff = rb(2025)

redbluff$Date = as.Date(redbluff$Date, format = "%Y-%m-%d")

redlong = redbluff %>%
  select(Project, Date, DataType, Winter.Chinook.BroodYear, Winter.Chinook.PassageEstimate, Spring.Chinook.BroodYear,
         Spring.Chinook.PassageEstimate, Fall.Chinook.BroodYear, Fall.Chinook.PassageEstimate,
         Late.Fall.Chinook.BroodYear, Late.Fall.Chinook.PassageEstimate) %>%
  pivot_longer(cols = c(Winter.Chinook.BroodYear:last_col()),names_sep = ".Chinook.", names_to = c("RunType", "Broodyear"),
               values_to =  "Value") %>%
  pivot_wider(names_from = Broodyear, values_from = Value) %>%
  group_by(BroodYear, RunType) %>%
  summarize(Passage = sum(PassageEstimate, na.rm =T))


#write the result
write.csv(redlong , file.path(data_root,"redbluff_all.csv"), row.names = FALSE)


############################################################################################################

#Now let's look at the GrandTab adult return data

#Try out Michael's spiffy new function

# Potential issue with conditioning on season == "Winter" when season has length > 1.
# Run seasons separately for now to be safe?
# Also, potential parsing problems that may need investigating?
##Grandtab = get_grandtab_data(season=c("Spring","Fall"))


spring = read_csv("http://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?outputFormat=csv&species=Chinook%3ASpring&type=In-River&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll")

fall = read_csv("http://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?outputFormat=csv&species=Chinook%3AFall&type=In-River&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll")

winter =  read_csv("http://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?outputFormat=csv&species=Chinook%3AWinter&type=All&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll")


Spring = spring %>%
  mutate(Year = as.numeric(str_sub(`End Year of Monitoring Period`, 1,4))) %>%
  filter(!is.na(Year)) %>%
  rename(Spring = `Population Estimate`) %>%
  select(Spring, Year)

Fall = fall%>%
  mutate(Year = as.numeric(str_sub(`End Year of Monitoring Period`, 1,4))) %>%
  filter(!is.na(Year)) %>%
  rename(Fall = `Population Estimate`) %>%
  select(Fall, Year)

Winter = winter%>%
  mutate(Year = as.numeric(str_sub(`End Year of Monitoring Period`, 1,4))) %>%
  filter(!is.na(Year)) %>%
  rename(Winter = `Population Estimate`) %>%
  select(Winter, Year)


AllAdults = left_join(Fall, Winter) %>%
  left_join(Spring)



#CSV of all the adult escapement
write.csv(AllAdults, file.path(data_root,"Grandtab_adultsalmon.csv"), row.names = F)

#save all these as a RData file
save(Spring, Winter, Fall, redlong , file = file.path(data_root, "Grantab.RData"))
