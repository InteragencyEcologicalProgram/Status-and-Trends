
#I really don't remember what this code was for. Why didn't I comment it? 

library(readxl)
library(ggplot2)
ZoopCPUETotals <- read_excel("C:/Users/rhartman/Desktop/ZoopCPUETotals.xlsx")
View(ZoopCPUETotals)
str(ZoopCPUETotals)


foo = data.frame(Date = ZoopCPUETotals$Date, totalCPUE = rnorm(nrow(ZoopCPUETotals), mean = 2000, sd = 1000))

ggplot()+
  geom_point(data = ZoopCPUETotals, aes(x=Date, y = totalCPUE))+
  geom_line(data = foo, aes(x=Date, y = totalCPUE))

boxplot(ZoopCPUETotals$totalCPUE)

boxplot(ZoopCPUETotals$totalCPUE, outline = T)

boxplot(totalCPUE~Station, data = ZoopCPUETotals)

library(ggplot2)

ggplot(ZoopCPUETotals, aes(y = totalCPUE, fill = Station)) + geom_boxplot()


ggplot(ZoopCPUETotals, aes(x=Date, y = totalCPUE, color = Station)) + geom_point()

ggplot(ZoopCPUETotals, aes(x=Date, y = totalCPUE, color = Station)) + geom_point()+
  facet_wrap(~Station)

FMWTl = pivot_longer(FMWT, cols =c("Aequorea spp (Lens Jellyfish)":"Yellowfin Goby"), 
                     names_to = "Species", values_to= "catch")


FMWT3 = filter(FMWTl, Year > 2000, Station > 700) %>%
               group_by(Year, Species) %>%
  summarize(tot = sum(catch))

FMWT4 =  group_by(FMWT3, Species) %>%
  summarize(tot2 = sum(tot), percent = tot2/sum(FMWT3$tot))

ggplot(FMWT3, aes(x = Year, y= tot, fill = Species)) + geom_bar(stat = "identity")

FMWTshrimp = filter(FMWTl, Species %in% c("Siberian Prawn", "Delta Smelt","American Shad", "Bluegill"), Survey %in% c(3,4,5), Year >1999) %>%
  group_by(Year, Species) %>%
summarize(tot = sum(catch))
ggplot(FMWTshrimp, aes(x = Year, y = tot)) + geom_bar(stat = "identity")

FMWTshad = filter(FMWTl, Species %in% c("American Shad"), Survey %in% c(3,4,5), Year >1999) %>%
  group_by(Year, Species) %>%
  summarize(tot = sum(catch))
ggplot(FMWTshad, aes(x = Year, y = tot)) + geom_bar(stat = "identity")


library(barplot3d)

barplot3d(rows = 4, cols = 20, #Four rows and 20 columns because we have four species and twenty years
          z = FMWTshrimp$tot, #Hight of the bars is equal to the total catch
          alpha = 0.5, #make the bars transparent
          theta = 20, phi = 20, #Rotate the graph so you can see it being 3-d
          scalexy = 1000, #make the colums width the same as having a column height of 1000 fish
          topcolors = rainbow(4), #make them different colors
          xlabels = c(2000:2019), #labels
          ylabels = c("Siberian Prawn", "Delta Smelt","American Shad", "Bluegill"))


FMWTsmelt = filter(FMWTl, Species == "Delta Smelt", Year > 2001) %>%
  group_by(Station) %>%
  summarise(Catch = mean(catch), PA = length(catch[which(catch != 0)]))

FMWTsmelt$Suisun = rep(NA, nrow(FMWTsmelt))
FMWTsmelt$Suisun[which(FMWTsmelt$Station %in% c("604", "605", "606", "608"))] = TRUE
FMWTsmelt$Suisun[-which(FMWTsmelt$Station %in% c("604", "605", "606", "608"))] = FALSE

smeltgraph = ggplot(FMWTsmelt, aes(x= Station, y = Catch, fill = Suisun)) +
  geom_bar(stat = "identity")
smeltgraph

smeltgraph2 = ggplot(FMWTsmelt, aes(x= Station, y = PA, fill = Suisun)) +
  geom_bar(stat = "identity")
smeltgraph2
