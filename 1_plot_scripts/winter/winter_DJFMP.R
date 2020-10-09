
## Starting with Ryan's script "DJFMP AR Chinook_Script.R".

CHNW_Trawl_DecJanFeb_Means <- CHNW_Trawl_CPUE %>%
	dplyr::filter(MONTH %in% c("Dec","Jan","Feb") & description == "Chipps Island") %>%
  group_by(FieldYear, MONTH, DAY,description,StationCode,Origin) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, DAY,description,Origin) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, description, Origin)%>%  ##mean monthly CPUE by site
  summarise(CPUE_month_site=mean(CPUE_day_site, na.rm = TRUE))%>%
	group_by(FieldYear, description, Origin) %>% ## mean CPUE by "season" (Dec - Feb)
	summarise(CPUE_season=mean(CPUE_month_site, na.rm = TRUE)) %>%
	ungroup() %>%
  filter(!is.nan(CPUE_season)) %>%
	mutate(FieldYear=as.numeric(as.character(FieldYear)),
				 ReportYear=(FieldYear - 1))

## Don't distinguish between Origin:
CHNW_Trawl_DecJanFeb_Mean_AllFish <- CHNW_Trawl_CPUE %>%
	dplyr::filter(MONTH %in% c("Dec","Jan","Feb") & description == "Chipps Island") %>%
  group_by(FieldYear, MONTH, DAY,description,StationCode) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, DAY,description) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, description)%>%  ##mean monthly CPUE by site
  summarise(CPUE_month_site=mean(CPUE_day_site, na.rm = TRUE))%>%
	group_by(FieldYear, description) %>% ## mean CPUE by "season" (Dec - Feb)
	summarise(CPUE_season=mean(CPUE_month_site, na.rm = TRUE)) %>%
	ungroup() %>%
  filter(!is.nan(CPUE_season)) %>%
	mutate(FieldYear=as.numeric(as.character(FieldYear)),
				 ReportYear=(FieldYear - 1))

CHNW_WinterSeason_Chipps<-ggplot(subset(CHNW_Trawl_DecJanFeb_Means, description=="Chipps Island"),aes(x=ReportYear,y=CPUE_season*10000, fill=Origin))+
  geom_bar(stat="identity", colour="black", size=0.5) + # Thin Black Outlines
  scale_fill_manual(values=CHNWcolors, labels=c("Marked Hatchery", "Unmarked Hatchery", "Wild"))+ #bar colors
  scale_y_continuous(expand = c(0, 0)) + #y axis scale
  #scale_x_continuous(breaks=seq(2000,2018,2))+
  #ggtitle("Delta Exit Trawl (Chipps Island)")+
	ylab(expression(paste("Average CPUE (fish/m"^"3"*" x 10,000)"))) + 
	xlab("Year (December - February)") + 
  theme(panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "white", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        #axis.title.x = element_blank(), 
        #axis.title.y = element_blank(), 
        axis.line = element_line(colour = "black"),
        text=element_text(size=11, colour = "#000000"),
        plot.title = element_text(hjust = 0.5, size = 11, colour = "#000000"),
        legend.position = "bottom", 
        legend.title = element_blank(),
				legend.text = element_text(size=7), 
        legend.key.size = unit(1, 'lines'))

CHNW_WinterSeason_Chipps_meanline <- CHNW_WinterSeason_Chipps + 
  geom_hline(yintercept=mean(CHNW_Trawl_DecJanFeb_Mean_AllFish$CPUE_season)*10000, 
						 col="red", linetype="dashed", size=0.9)


ggsave(CHNW_WinterSeason_Chipps, file="djfmp_chnw_chipps_winter.png",
       dpi=300, units="cm", width=9.3, height=10)  # 6.8

ggsave(CHNW_WinterSeason_Chipps_meanline, file="djfmp_chnw_chipps_winter_meanline.png", 
			 dpi=300, units="cm", width=9.3, height=10)



# ## Each record represents a unique combination of (haul, Organism, MarkCode, StageCode, 
# ## Maturation, FL, RaceByLength) with a Count for the total number of organisms.
# beach_seine_file <- file.path("data","archive_edi.244.3",
                          # "1976-2018_DJFMP_beach_seine_fish_and_water_quality_data.csv")
# taxonomy_file <- file.path("data","archive_edi.244.3","DJFMP_Fish_Taxonomy.csv")
# site_file <- file.path("data","archive_edi.244.3","DJFMP_Site_Locations.csv")


# beach_seine_df <- read.csv(beach_seine_file, stringsAsFactors=FALSE)
# taxonomy_df <- read.csv(taxonomy_file, stringsAsFactors=FALSE)
# site_df <- read.csv(site_file, stringsAsFactors=FALSE)

# # str(beach_seine_df)
# # str(taxonomy_df)
# # str(site_df)


# ## Filter dates, years, regions, and/or stations?
# beach_seine_df$NewRegion <- NA
# beach_seine_df$NewRegion[beach_seine_df$RegionCode %in% c(1,7)] <- "Sacramento River"
# beach_seine_df$NewRegion[beach_seine_df$RegionCode %in% 2:4] <- "Delta"
# beach_seine_df$NewRegion[beach_seine_df$RegionCode == 5] <- "San Joaquin River"
# beach_seine_df$NewRegion[beach_seine_df$RegionCode == 6] <- "San Pablo Bay"

# beach_seine_sub <- beach_seine_df %>%
  # mutate(SampleDate=as.Date(as.character(SampleDate)),
         # Year=lubridate::year(SampleDate),
         # Month=lubridate::month(SampleDate)) %>%
  # filter(NewRegion %in% c("San Pablo Bay","Delta") & Month %in% 1:3)
  # # # filter(RegionCode != 7 & Year >= 1999 & Month %in% 1:3)
  # # Ryan said region 7 (Sac) is seasonal.


# ## Calculate CPUE by year-region (average over months):
# bs_cpue_df <- beach_seine_sub %>%
  # group_by(Year, NewRegion, MethodCode) %>%
  # summarize(CHN_CPUE=10000*sum(Count[OrganismCode == "CHN"])/sum(Volume),
            # SPLT_CPUE=10000*sum(Count[OrganismCode == "SPLT"])/sum(Volume),
            # Total_Volume=sum(Volume)
           # ) %>%
  # ungroup() %>%
  # data.frame()


# # djfmp_chn_fig <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=CHN_CPUE, fill=NewRegion)) + 
  # # geom_bar(stat="identity", position=position_stack()) + 
  # # theme_iep() + 
  # # theme(legend.position="bottom") + 
  # # facet_wrap( ~ NewRegion, nrow=2, scales="free_y") + 
  # # scale_fill_discrete(name="Region") + 
  # # scale_y_continuous(expression(paste("Chinook CPUE (fish/m"^"3"*" x 10,000)")))
  # # # scale_y_continuous("Chinook CPUE (fish/m^3 x 10,000)")



# ## Plot option 1 (start in 1966):
# year_min <- min(bs_cpue_df$Year)
# year_max <- max(bs_cpue_df$Year)

# bs_cpue_df$dummy_y <- 0
# missing_vol <- subset(bs_cpue_df, is.na(Total_Volume))
# zero_CHN <- subset(bs_cpue_df, CHN_CPUE == 0)
# zero_SPLT <- subset(bs_cpue_df, SPLT_CPUE == 0)

# missing_vol$type <- "No volume data"
# zero_CHN$type <- "Zero catch"
# zero_SPLT$type <- "Zero catch"

# missing_vol_or_zero_CHN <- rbind(missing_vol, zero_CHN)
# missing_vol_or_zero_SPLT <- rbind(missing_vol, zero_SPLT)


# djfmp_chn_fig_1966 <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=CHN_CPUE)) + 
  # geom_bar(stat="identity", position=position_stack()) + 
  # theme_iep() + 
  # theme(legend.position="bottom") + 
  # facet_wrap( ~ NewRegion, nrow=2, scales="free_x") + #, scales="free_y") + 
  # scale_y_continuous(expression(paste("Chinook CPUE (fish/m"^"3"*" x 10,000)"))) + 
  # xlim(1966, year_max) + 
  # annotate("text", label=paste0("Data were not collected until ",year_min,"."), x=1980, 
            # y=500, size=2.5) + 
  # geom_point(data=missing_vol_or_zero_CHN, aes(x=Year, y=dummy_y, shape=type)) + 
  # scale_shape_discrete(name="") + 
  # theme(legend.box.background=element_rect(colour="black"))
  
# djfmp_splt_fig_1966 <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=SPLT_CPUE)) + 
  # geom_bar(stat="identity", position=position_stack()) + 
  # theme_iep() + 
  # theme(legend.position="bottom") + 
  # facet_wrap( ~ NewRegion, nrow=2, scales="free_x") + #, scales="free_y") + 
  # scale_y_continuous(expression(paste("Splittail CPUE (fish/m"^"3"*" x 10,000)"))) + 
  # xlim(1966, year_max) + 
  # annotate("text", label=paste0("Data were not collected until ",year_min,"."), x=1980, 
            # y=20, size=2.5) + 
  # geom_point(data=missing_vol_or_zero_SPLT, aes(x=Year, y=dummy_y, shape=type)) + 
  # scale_shape_discrete(name="") + 
  # theme(legend.box.background=element_rect(colour="black"))
  


# ## Plot option 2 (2003 - 2018):

# # Was leaving out the Delta 2003 bar when I used xlim(2003, 2018).
# djfmp_chn_fig_2003 <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=CHN_CPUE)) + 
  # geom_bar(stat="identity", position=position_stack()) + 
  # theme_iep() + 
  # theme(legend.position="bottom") + 
  # facet_wrap( ~ NewRegion, nrow=2, scales="free_x") + #, scales="free_y") + 
  # scale_y_continuous(expression(paste("Chinook CPUE (fish/m"^"3"*" x 10,000)"))) + 
  # xlim(2002.5, 2018) + 
  # geom_point(data=missing_vol_or_zero_CHN, aes(x=Year, y=dummy_y, shape=type)) + 
  # scale_shape_discrete(name="") + 
  # theme(legend.box.background=element_rect(colour="black"))

# djfmp_splt_fig_2003 <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=SPLT_CPUE)) + 
  # geom_bar(stat="identity", position=position_stack()) + 
  # theme_iep() + 
  # theme(legend.position="bottom") + 
  # facet_wrap( ~ NewRegion, nrow=2, scales="free_x") + #, scales="free_y") + 
  # scale_y_continuous(expression(paste("Splittail CPUE (fish/m"^"3"*" x 10,000)"))) + 
  # xlim(2002.5, 2018) + 
  # annotate("text", label=paste0("Data were not collected until ",year_min,"."), x=1980, 
            # y=20, size=2.5) + 
  # geom_point(data=missing_vol_or_zero_SPLT, aes(x=Year, y=dummy_y, shape=type)) + 
  # scale_shape_discrete(name="") + 
  # theme(legend.box.background=element_rect(colour="black"))




# ggsave(djfmp_chn_fig_1966, file="djfmp_chn_fig_1966.png", path=salmon_fig_root, 
       # dpi=300, units="cm", width=9.3, height=10)  # 6.8

# ggsave(djfmp_splt_fig_1966, file="djfmp_splt_fig_1966.png", path=otherfish_fig_root, 
       # dpi=300, units="cm", width=9.3, height=10)


# ggsave(djfmp_chn_fig_2003, file="djfmp_chn_fig_2003.png", path=salmon_fig_root, 
       # dpi=300, units="cm", width=9.3, height=10)

# ggsave(djfmp_splt_fig_2003, file="djfmp_splt_fig_2003.png", path=otherfish_fig_root, 
       # dpi=300, units="cm", width=9.3, height=10)





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


