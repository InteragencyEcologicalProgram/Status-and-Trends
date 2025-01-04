
library(tidyverse)
library(lubridate)

p_spch <- ggplot(Spring, aes(x=Year, y=sprinrun/1000))+
  geom_line(stat="identity", size = 1, color = "darkblue") +
  theme_bw()+
  theme(legend.position="none") + 
  smr_y_axis(name = "Spring Run Chinook Adult Returns (thousands)") +
 xlab("Year") 
p_spch



p_fch <- ggplot(Fall, aes(x=Year, y=fallrun/1000))+
  geom_line(stat="identity", size = 1, color = "darkred") +
  theme_bw()+
  theme(legend.position="none") + 
  smr_y_axis(name = "Fall Run Chinook Adult Returns (thousands)") +
  xlab("Year")
p_fch

p_wch <- ggplot(Winter, aes(x=Year, y=Annual/1000))+
  geom_line(stat="identity", size = 1, color = "darkgreen") +
  theme(legend.position="none") +
  theme_bw()+
  smr_y_axis(name = "Winter Run Chinook Adult Returns (thousands)") +
  xlab("Year")
p_wch

p_lfch <- ggplot(Latefall, aes(x=Year, y=Annual/1000))+
  geom_line(stat="identity", size = 1, color = "darkgreen") +
  theme(legend.position="none") +
  theme_bw()+
  smr_y_axis(name = "Late Fall Run Chinook Adult Returns (thousands)") +
  xlab("Year")
p_lfch

#put all the slamon together in one graph
Fall = mutate(Fall, runname = "Fall", Annual = fallrun, fallrun = NULL)
Spring = mutate(Spring, runname = "Spring", Annual = sprinrun, sprinrun = NULL)
Winter = mutate(Winter, runname = "Winter")
Latefall = mutate(Latefall, runname = "LateFall")
Salmon = bind_rows(Fall, Winter, Latefall, Spring, )


p_sal <- ggplot(Salmon, aes(x=Year, y=Annual/1000))+
  geom_area(stat="identity", size = 1, aes(fill = runname)) +
  theme(legend.position="none") +
  theme_bw()+# facet_wrap(~runname)+
  smr_y_axis(name = "Chinook Salmon Adult Returns (Thousands)") +
  xlab("Year")
p_sal



#########################################################
#smelt
#Traditional Index 1967-2019
p_ds <- ggplot(ds, aes(x=Year, y=Index))+
  geom_line(size = 1, color = "darkorange") +
  theme_bw()+
  theme(legend.position="none") + 
  scale_y_continuous("Delta Smelt Index") +
  xlab("Year") 
p_ds


#Longfin Smelt


#Traditional Index 1967-2019
p_lfs <- ggplot(lfs, aes(x=Year, y=Index))+
  geom_line(size = 1, color = "brown") +
  theme_bw()+
  xlab("Year")+
  theme(legend.position="none") + 
  scale_y_continuous("Longfin Smelt Index")
  
p_lfs  

p_ds <- ggplot(ds, aes(x=Year, y=Index))+
  geom_line(size = 1, color = "darkorange") +
  theme_bw()+
  theme(legend.position="none") + 
  scale_y_continuous("Delta Smelt Index") +
  xlab("Year") 
p_ds


#Longfin Smelt

#Traditional Index 2000-2019
lfs2 <- ggplot(filter(lfs, Year >1999), aes(x=Year, y=Index))+
  geom_line(size = 1, color = "brown") +
  theme_bw()+
  xlab("Year")+
  theme(legend.position="none") + 
  scale_y_continuous("Longfin Smelt Index")

lfs2  



p_ds2 <- ggplot(filter(ds, Year >1999), aes(x=Year, y=Index))+
  geom_line(size = 1, color = "darkorange") +
  theme_bw()+
  theme(legend.position="none") + 
  scale_y_continuous("Delta Smelt Index") +
  xlab("Year") 
p_ds2

########################################################
#Someone asked if we could overlay drought conditions
WYs_1906_2020 <- read_csv("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/DroughtSynthesis/data/WYs_1906-2020.csv")
WY = mutate(WYs_1906_2020, Year = WY) %>%
  filter(Year >1967)


p_ds3 <- ggplot()+
  geom_col(data = WY, aes(x = Year, y = 1500, fill = Drought), alpha = 0.2)+
  scale_fill_manual(values = c("white", "darkred"), labels = c("not drought", "drought"))+
  geom_line(data = ds, aes(x=Year, y=Index),size = 1, color = "darkorange") +
  theme_bw()+
  scale_y_continuous("Delta Smelt Index") +
  xlab("Year") 
p_ds3


p_lf4 <- ggplot()+
  geom_col(data = WY, aes(x = Year, y = 80000, fill = Drought), alpha = 0.2)+
  scale_fill_manual(values = c("white", "darkred"), labels = c("not drought", "drought"))+
  geom_line(data = lfs, aes(x=Year, y=Index),size = 1, color = "brown") +
  theme_bw()+
  scale_y_continuous("Longfin Smelt Index") +
  xlab("Year") 
p_lf4
