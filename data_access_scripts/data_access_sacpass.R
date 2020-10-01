#download relevant datasets from sacpass

#http://www.cbr.washington.edu/sacramento/


#First I"ll look at redbluff
#unfortunately, I need to dowload it one year at a time.

library(tidyverse)
library(lubridate)
library(smonitr)



urls =  "http://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_daily.php?outputFormat=csv&year=2019&biweekly=other&wtemp=default"

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

redbluff = rb(2019)

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
write.csv(Redlong, "data/redbluff_all.csv", row.names = FALSE)


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


SpringURL = "http://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?outputFormat=csv&species=Chinook%3ASpring&type=In-River&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll"
FallURL = "http://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?outputFormat=csv&species=Chinook%3AFall&type=In-River&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll"
WinterURL = "http://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?outputFormat=csv&species=Chinook%3AWinter&type=All&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll"



Spring = read.csv(SpringURL, stringsAsFactors = F)
Spring = Spring[1:(which(Spring$Year == "Notes:")-1),]

#convert the year to a number
Spring = mutate(Spring, Year = as.numeric(substr(Year, 1, 4))) %>%
  rename(sprinrun = Annual)


#fall run
Fall = read.csv(FallURL, stringsAsFactors = F)
Fall = Fall[1:(which(Fall$Year == "Notes:")-1),]

#convert the year to a number
Fall = mutate(Fall, Year = as.numeric(substr(Year, 1, 4))) %>%
  rename(FallRun = Annual)

#winter run
Winter = read.csv(WinterURL, stringsAsFactors = F)
Winter = Winter[1:(which(Winter$Year == "Notes:")-1),]

#convert the year to a number
Winter = mutate(Winter, Year = as.numeric(substr(Year, 1, 4))) %>%
  rename(WinterRun = Annual)

AllAdults = left_join(Fall, Winter) %>%
  left_join(Spring)

#CSV of all the adult escapement
write.csv(AllAdults, "data/Grandtab_adultsalmon.csv", row.names = F)


get_grandtab_data2 = function (season = c("Winter", "Spring", "Fall", 
                     "Late-Fall"), parse_fun, ..., verbose = TRUE) 
{
  if (missing(parse_fun)) {
    parse_fun = default_parse_fun(verbose)
  }
  else if (!is.function(parse_fun)) {
    stop("argument \"parse_fun\" must be a function.")
  }
  if (!all(season %in% c("Winter", "Spring", "Fall", 
                         "Late-Fall"))) {
    stop("Unrecognized value in argument \"seasons\"")
  }
  species = "Chinook"
  if (season == "Winter") spawn_type = "All" else spawn_type = "In-River"
  spawn_location = str_replace_all("Sacramento and San Joaquin River Systems", 
                                   " ", "+")
  if (verbose) {
    message("Downloading data for ", paste(shQuote(season), 
                                           collapse = ", "), ".")
  }
  urls = glue("http://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?outputFormat=csv&species={species}%3A{season}&type={spawn_type}&locType=location&location={spawn_location}%3AAll%3AAll")
  grandtab.raw = map(urls, parse_fun, ...)
  names(grandtab.raw) = season
  err = map_lgl(grandtab.raw, ~!all(names(.x) %in% c("Year", 
                                                     "Annual")))
  if (any(err)) {
    stop("Error retrieving data for season: ", paste(shQuote(season[err]), 
                                                     collapse = ", "), ".", call. = FALSE)
    grandtab.raw[season[err]] = NULL
  }
  notes.index = map(grandtab.raw, ~which(str_detect(.x[["Year"]], 
                                                    "Notes:")))
  grandtab = map2(grandtab.raw, notes.index, ~head(.x, .y - 
                                                     1))
  grandtab = map(grandtab, ~mutate(.x, flag = ifelse(str_detect(.data$Year, 
                                                                "\\*"), "*", ""), Year = as.integer((str_replace(.data$Year, 
                                                                                                                 "\\*", "")))))
  grandtab.notes = map2(grandtab.raw, notes.index, ~str_c(tail(.x[["Year"]], 
                                                               -.y), collapse = "\n"))
  attr(grandtab, "Notes") = grandtab.notes
  grandtab
}

winter = get_grandtab_data2(season = "winter")
