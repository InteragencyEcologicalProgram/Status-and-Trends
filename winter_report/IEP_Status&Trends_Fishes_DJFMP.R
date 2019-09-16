
library(ggplot2)
library(tidyverse)


## Each record represents a unique combination of (haul, Organism, MarkCode, StageCode, 
## Maturation, FL, RaceByLength) with a Count for the total number of organisms.
beach_seine_file <- file.path("data","archive_edi.244.3",
                          "1976-2018_DJFMP_beach_seine_fish_and_water_quality_data.csv")
taxonomy_file <- file.path("data","archive_edi.244.3","DJFMP_Fish_Taxonomy.csv")
site_file <- file.path("data","archive_edi.244.3","DJFMP_Site_Locations.csv")


beach_seine_df <- read.csv(beach_seine_file, stringsAsFactors=FALSE)
taxonomy_df <- read.csv(taxonomy_file, stringsAsFactors=FALSE)
site_df <- read.csv(site_file, stringsAsFactors=FALSE)

# str(beach_seine_df)
# str(taxonomy_df)
# str(site_df)


## Filter dates, years, regions, and/or stations?
beach_seine_df$NewRegion <- NA
beach_seine_df$NewRegion[beach_seine_df$RegionCode %in% c(1,7)] <- "Sacramento River"
beach_seine_df$NewRegion[beach_seine_df$RegionCode %in% 2:4] <- "Delta"
beach_seine_df$NewRegion[beach_seine_df$RegionCode == 5] <- "San Joaquin River"
beach_seine_df$NewRegion[beach_seine_df$RegionCode == 6] <- "San Pablo Bay"

beach_seine_sub <- beach_seine_df %>%
  mutate(SampleDate=as.Date(as.character(SampleDate)),
         Year=lubridate::year(SampleDate),
         Month=lubridate::month(SampleDate)) %>%
  filter(NewRegion %in% c("San Pablo Bay","Delta") & Month %in% 1:3)
  # # filter(RegionCode != 7 & Year >= 1999 & Month %in% 1:3)
  # Ryan said region 7 (Sac) is seasonal.


## Calculate CPUE by year-region (average over months):
bs_cpue_df <- beach_seine_sub %>%
  group_by(Year, NewRegion, MethodCode) %>%
  summarize(CHN_CPUE=10000*sum(Count[OrganismCode == "CHN"])/sum(Volume),
            SPLT_CPUE=10000*sum(Count[OrganismCode == "SPLT"])/sum(Volume),
            Total_Volume=sum(Volume)
           ) %>%
  ungroup() %>%
  data.frame()


# djfmp_chn_fig <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=CHN_CPUE, fill=NewRegion)) + 
  # geom_bar(stat="identity", position=position_stack()) + 
  # theme_iep() + 
  # theme(legend.position="bottom") + 
  # facet_wrap( ~ NewRegion, nrow=2, scales="free_y") + 
  # scale_fill_discrete(name="Region") + 
  # scale_y_continuous(expression(paste("Chinook CPUE (fish/m"^"3"*" x 10,000)")))
  # # scale_y_continuous("Chinook CPUE (fish/m^3 x 10,000)")



## Plot option 1 (start in 1966):
year_min <- min(bs_cpue_df$Year)
year_max <- max(bs_cpue_df$Year)

bs_cpue_df$dummy_y <- 0
missing_vol <- subset(bs_cpue_df, is.na(Total_Volume))
zero_CHN <- subset(bs_cpue_df, CHN_CPUE == 0)
zero_SPLT <- subset(bs_cpue_df, SPLT_CPUE == 0)

missing_vol$type <- "No volume data"
zero_CHN$type <- "Zero catch"
zero_SPLT$type <- "Zero catch"

missing_vol_or_zero_CHN <- rbind(missing_vol, zero_CHN)
missing_vol_or_zero_SPLT <- rbind(missing_vol, zero_SPLT)


djfmp_chn_fig_1966 <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=CHN_CPUE)) + 
  geom_bar(stat="identity", position=position_stack()) + 
  theme_iep() + 
  theme(legend.position="bottom") + 
  facet_wrap( ~ NewRegion, nrow=2, scales="free_x") + #, scales="free_y") + 
  scale_y_continuous(expression(paste("Chinook CPUE (fish/m"^"3"*" x 10,000)"))) + 
  xlim(1966, year_max) + 
  annotate("text", label=paste0("Data were not collected until ",year_min,"."), x=1980, 
            y=500, size=2.5) + 
  geom_point(data=missing_vol_or_zero_CHN, aes(x=Year, y=dummy_y, shape=type)) + 
  scale_shape_discrete(name="") + 
  theme(legend.box.background=element_rect(colour="black"))
  
djfmp_splt_fig_1966 <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=SPLT_CPUE)) + 
  geom_bar(stat="identity", position=position_stack()) + 
  theme_iep() + 
  theme(legend.position="bottom") + 
  facet_wrap( ~ NewRegion, nrow=2, scales="free_x") + #, scales="free_y") + 
  scale_y_continuous(expression(paste("Splittail CPUE (fish/m"^"3"*" x 10,000)"))) + 
  xlim(1966, year_max) + 
  annotate("text", label=paste0("Data were not collected until ",year_min,"."), x=1980, 
            y=20, size=2.5) + 
  geom_point(data=missing_vol_or_zero_SPLT, aes(x=Year, y=dummy_y, shape=type)) + 
  scale_shape_discrete(name="") + 
  theme(legend.box.background=element_rect(colour="black"))
  


## Plot option 2 (2003 - 2018):

# Was leaving out the Delta 2003 bar when I used xlim(2003, 2018).
djfmp_chn_fig_2003 <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=CHN_CPUE)) + 
  geom_bar(stat="identity", position=position_stack()) + 
  theme_iep() + 
  theme(legend.position="bottom") + 
  facet_wrap( ~ NewRegion, nrow=2, scales="free_x") + #, scales="free_y") + 
  scale_y_continuous(expression(paste("Chinook CPUE (fish/m"^"3"*" x 10,000)"))) + 
  xlim(2002.5, 2018) + 
  geom_point(data=missing_vol_or_zero_CHN, aes(x=Year, y=dummy_y, shape=type)) + 
  scale_shape_discrete(name="") + 
  theme(legend.box.background=element_rect(colour="black"))

djfmp_splt_fig_2003 <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=SPLT_CPUE)) + 
  geom_bar(stat="identity", position=position_stack()) + 
  theme_iep() + 
  theme(legend.position="bottom") + 
  facet_wrap( ~ NewRegion, nrow=2, scales="free_x") + #, scales="free_y") + 
  scale_y_continuous(expression(paste("Splittail CPUE (fish/m"^"3"*" x 10,000)"))) + 
  xlim(2002.5, 2018) + 
  annotate("text", label=paste0("Data were not collected until ",year_min,"."), x=1980, 
            y=20, size=2.5) + 
  geom_point(data=missing_vol_or_zero_SPLT, aes(x=Year, y=dummy_y, shape=type)) + 
  scale_shape_discrete(name="") + 
  theme(legend.box.background=element_rect(colour="black"))




ggsave(djfmp_chn_fig_1966, file="djfmp_chn_fig_1966.png", path=salmon_fig_root, 
       dpi=300, units="cm", width=9.3, height=10)  # 6.8

ggsave(djfmp_splt_fig_1966, file="djfmp_splt_fig_1966.png", path=otherfish_fig_root, 
       dpi=300, units="cm", width=9.3, height=10)


ggsave(djfmp_chn_fig_2003, file="djfmp_chn_fig_2003.png", path=salmon_fig_root, 
       dpi=300, units="cm", width=9.3, height=10)

ggsave(djfmp_splt_fig_2003, file="djfmp_splt_fig_2003.png", path=otherfish_fig_root, 
       dpi=300, units="cm", width=9.3, height=10)





# ## Number of unique regions by year-month:
# tapply(bs_cpue_df$NewRegion, list(bs_cpue_df$Month, bs_cpue_df$Year), 
  # function(x) {
    # length(unique(x))
  # }
# )

# ## Which unique regions by year-month:
# lapply(split(bs_cpue_df$NewRegion, list(bs_cpue_df$Month, bs_cpue_df$Year)),
  # function(x) {
    # sort(unique(x))
  # }
# )

# ## Look at subsets of years and months:
# tmp <- subset(bs_cpue_df, Year >= 1980 & Month %in% 1:3)
# lapply(split(tmp$NewRegion, list(tmp$Month, tmp$Year)),
  # function(x) {
    # sort(unique(x))
  # }
# )

# tmp1 <- subset(bs_cpue_df, Year >= 1993 & Month %in% c(1,2,3,12))
# lapply(split(tmp1$RegionCoce, list(tmp1$Month, tmp1$Year)),
  # function(x) {
    # sort(unique(x))
  # }
# )

# ## Look at unique stations by year-month (for subsets of year and month):
# tmp <- subset(bs_cpue_df, Year >= 1980 & Month %in% 1:3)
# a <- lapply(split(tmp$StationCode, list(tmp$Month, tmp$Year)),
  # function(x) {
    # sort(unique(x))
  # }
# )
# aa <- intersect(a[[1]],a[[2]])
# for(i in 3:length(a)) {
  # if(names(a)[i] != "1.1995") {
    # aa <- intersect(aa, a[[i]])
  # }
# }

# tmp1 <- subset(bs_cpue_df, Year >= 1993 & Month %in% c(1,2,3,12))
# a1 <- lapply(split(tmp1$StationCode, list(tmp1$Month, tmp1$Year)),
  # function(x) {
    # sort(unique(x))
  # }
# )
# aa1 <- intersect(a1[[1]], a1[[2]])
# for(i in 3:length(a1)) {
  # aa1 <- intersect(aa1, a1[[i]])
# }
# aa1


