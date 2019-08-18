
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
beach_seine_sub <- beach_seine_df %>%
  mutate(SampleDate=as.Date(as.character(SampleDate)),
         Year=lubridate::year(SampleDate),
         Month=lubridate::month(SampleDate)) %>%
  filter(RegionCode != 7 & Year >= 1999 & Month %in% 1:3)
  # Ryan said region 7 (Sac) is seasonal.


## Calculate CPUE by year-region (average over months):
bs_cpue_df <- beach_seine_sub %>%
  group_by(Year, RegionCode, MethodCode) %>%
  summarize(CHN_CPUE=10000*sum(Count[OrganismCode == "CHN"])/sum(Volume),
            SPLT_CPUE=10000*sum(Count[OrganismCode == "SPLT"])/sum(Volume)
           ) %>%
  ungroup() %>%
  data.frame()


djfmp_chn_fig <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=CHN_CPUE, fill=RegionCode)) + 
  geom_bar(stat="identity", position=position_stack()) + 
  theme_iep() + 
  theme(legend.position="bottom") + 
  scale_y_continuous(expression(paste("Chinook CPUE (fish/m"^"3"*" x 10,000)")))
  # scale_y_continuous("Chinook CPUE (fish/m^3 x 10,000)")

djfmp_splt_fig <- ggplot2::ggplot(bs_cpue_df, aes(x=Year, y=SPLT_CPUE, fill=RegionCode)) + 
  geom_bar(stat="identity", position=position_stack()) + 
  theme_iep() + 
  theme(legend.position="bottom") + 
  scale_y_continuous(expression(paste("Splittail CPUE (fish/m"^"3"*" x 10,000)")))
  # scale_y_continuous("Splittail CPUE (fish/m^3 x 10,000)")


ggsave(djfmp_chn_fig, file="djfmp_chn_fig.png", path=salmon_fig_root, scale=2,
       dpi=300, units="cm", width=8.5, height=5.5)

ggsave(djfmp_splt_fig, file="djfmp_splt_fig.png", path=otherfish_fig_root, scale=2,
       dpi=300, units="cm", width=8.5, height=5.5)




# ## Number of unique regions by year-month:
# tapply(bs_cpue_df$RegionCode, list(bs_cpue_df$Month, bs_cpue_df$Year), 
  # function(x) {
    # length(unique(x))
  # }
# )

# ## Which unique regions by year-month:
# lapply(split(bs_cpue_df$RegionCode, list(bs_cpue_df$Month, bs_cpue_df$Year)),
  # function(x) {
    # sort(unique(x))
  # }
# )

# ## Look at subsets of years and months:
# tmp <- subset(bs_cpue_df, Year >= 1980 & Month %in% 1:3)
# lapply(split(tmp$RegionCode, list(tmp$Month, tmp$Year)),
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


