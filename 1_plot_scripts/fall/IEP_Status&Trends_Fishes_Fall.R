#IEP Status and Trends Report
#Fall Season
#Fish plots panel (six graphs)
#FMWT: Delta smelt, longfin smelt, striped bass, American shad
#also white sturgeon, fall run chinook salmon

#Created by Nick Rasmussen
#udated by Rosemary Hartman 2/18/2020

source("setup.R")

#packages
library(ggplot2) 
library(cowplot) #plot_grid()
library(tidyverse)
library(smonitr)
library(readr)

#import data

#fall midwater trawl: Delta smelt, longfin smelt, striped bass, American shad
fmwt<-read_csv(file.path(data_root,"fmwt.csv"))
str(fmwt)

#go from long to wide
fmwt = pivot_longer(fmwt, cols = `Threadfin Shad`:`Striped Bass Age0`, 
                    names_to = "Species", values_to = "Index")

#remove years with "NAs" so that the missing data symbol works, 
fmwt = filter(fmwt, !is.na(Index)) 

#CA Fish & Wildlife: Sturgeon Trammel Net Surveys
wst<-read.csv(file.path(data_root,"Trammel_Net_CPUE_05052023.csv"))
str(wst)
wst = mutate(wst, fyear = as.factor(RelYear), Month = month(RelDate)) %>%
  filter(Month %in% c(9,10,11))
wstsum = group_by(wst, RelYear, fyear, Month, RelDate) %>%
  summarize(CPUE = mean(daily_cpue_white)) %>%
  group_by(RelYear, fyear) %>%
  summarize(CPUE = mean(CPUE, na.rm =T))

### Plot Fall Midwater Trawl species -----------------------------------

#create subsets
ds<-subset(fmwt,Species=="Delta Smelt")
lfs<-subset(fmwt,Species=="Longfin Smelt")
sb0<-subset(fmwt,Species=="Striped Bass Age0")
as<-subset(fmwt,Species=="American Shad")
 

# Delta Smelt, all years
delta_smelt_all_years <- ggplot(ds, aes(x=Year, y=Index)) +
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("Delta Smelt Index") +
  smr_x_axis(report_year, type = "all", season = "fall") +
	stat_missing(size=2.5) + 
  stat_lt_avg() + 
	smr_caption(stat_name="the Delta Smelt index", report_year=report_year) + 
	smr_alttext(stat_name="Delta Smelt index")

delta_smelt_all_years

getCaption(delta_smelt_all_years)
getAlttext(delta_smelt_all_years)


# Longfin Smelt, all years
longfin_smelt_all_years <- ggplot(lfs, aes(x=Year, y=Index)) +
	geom_bar(stat="identity") +
	smr_theme_update() + 
	smr_x_axis(report_year, type = "all", season = "fall") +
	scale_y_continuous("Longfin Smelt Index") +
	stat_missing(size=2.5) + 
	stat_lt_avg() + 
	smr_caption(stat_name="the Longfin Smelt index", report_year=report_year) + 
	smr_alttext(stat_name="Longfin Smelt index")

longfin_smelt_all_years

getCaption(longfin_smelt_all_years)
getAlttext(longfin_smelt_all_years)


# Striped Bass, all years
striped_bass0_all_years <- ggplot(sb0, aes(x=Year, y=Index)) +
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("Striped Bass Index") +
  smr_x_axis(report_year, type = "all", season = "fall") +
	stat_missing(size=2.5) + 
  stat_lt_avg() + 
	smr_caption(stat_name="the young-of-the-year striped bass index", 
							report_year=report_year) + 
	smr_alttext(stat_name="young-of-the-year striped bass index")

striped_bass0_all_years

getCaption(striped_bass0_all_years)
getAlttext(striped_bass0_all_years)


# American Shad, all years
american_shad_all_years <- ggplot(as, aes(x=Year, y=Index)) +
	geom_bar(stat="identity") +
	smr_theme_update() + 
	scale_y_continuous("American Shad Index")+
	smr_x_axis(report_year, type = "all", season = "fall") +
	stat_missing(size=2.5) + 
	stat_lt_avg() + 
	smr_caption(stat_name="the American Shad index", report_year=report_year) + 
	smr_alttext(stat_name="American Shad index")

american_shad_all_years

getCaption(american_shad_all_years)
getAlttext(american_shad_all_years)


## Recent trends for fish

# Delta Smelt, recent years
delta_smelt_recent <- ggplot(ds, aes(x=Year, y=Index)) +
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("Delta Smelt Index") +
  smr_x_axis(report_year, type = "recent", season = "fall") +
	stat_missing(size=2.5) + 
  stat_lt_avg() + 
	smr_caption(stat_name="the Delta Smelt index", report_year=report_year) + 
	smr_alttext(stat_name="Delta Smelt index")

delta_smelt_recent

getCaption(delta_smelt_recent)
getAlttext(delta_smelt_recent)


# Longfin Smelt, recent years
longfin_smelt_recent <- ggplot(lfs, aes(x=Year, y=Index)) +
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("Longfin Smelt Index") +
  smr_x_axis(report_year, type = "recent", season = "fall") +
	stat_missing(size=2.5) + 
  stat_lt_avg() + 
	smr_caption(stat_name="the Longfin Smelt index", report_year=report_year) + 
	smr_alttext(stat_name="Longfin Smelt index")

longfin_smelt_recent

getCaption(longfin_smelt_recent)
getAlttext(longfin_smelt_recent)


# Striped Bass, recent years
striped_bass0_recent <- ggplot(sb0, aes(x=Year, y=Index)) +
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("Striped Bass Index") +
  smr_x_axis(report_year, type = "recent", season = "fall") +
	stat_missing(size=2.5) + 
  stat_lt_avg() + 
	smr_caption(stat_name="the young-of-the-year striped bass index", 
							report_year=report_year) + 
	smr_alttext(stat_name="young-of-the-year striped bass index")

striped_bass0_recent

getCaption(striped_bass0_recent)
getAlttext(striped_bass0_recent)


# American Shad, recent years
american_shad_recent <- ggplot(as, aes(x=Year, y=Index)) +
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("American Shad Index") +
  smr_x_axis(report_year, type = "recent", season = "fall") +
	stat_missing(size=2.5) + 
  stat_lt_avg() + 
	smr_caption(stat_name="the American Shad index", report_year=report_year) + 
	smr_alttext(stat_name="American Shad index")

american_shad_recent

getCaption(american_shad_recent)
getAlttext(american_shad_recent)


### Plot Sturgeon Trammel Net Surveys species -----------------------------------

# White Sturgeon, all years
# unlike FMWT, these data actually start 1968 instead of 1966
white_sturgeon_all_years <- ggplot(wstsum, aes(x=RelYear, y=CPUE)) +
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("White Sturgeon CPUE") +
  smr_x_axis(report_year, type = "all", season = "fall") +
	stat_missing(size=2.5) + 
  stat_lt_avg() + 
	smr_caption(stat_name="the white sturgeon catch-per-unit-effort", 
							report_year=report_year) + 
	smr_alttext(stat_name="white sturgeon catch-per-unit-effort")

white_sturgeon_all_years

getCaption(white_sturgeon_all_years)
getAlttext(white_sturgeon_all_years)


# White Sturgeon, recent years
white_sturgeon_recent <- ggplot(wstsum, aes(x=RelYear, y=CPUE)) +
  geom_bar(stat="identity") +
  smr_theme_update() + 
  scale_y_continuous("White Sturgeon CPUE") +
  smr_x_axis(report_year, type = "recent", season = "fall") +
	stat_missing(size=2.5) + 
  stat_lt_avg() + 
	smr_caption(stat_name="the white sturgeon catch-per-unit-effort", 
							report_year=report_year) + 
	smr_alttext(stat_name="white sturgeon catch-per-unit-effort")

white_sturgeon_recent

getCaption(white_sturgeon_recent)
getAlttext(white_sturgeon_recent)


### Save plots -----------------------------------

fish_fall <- list()
fish_fall[["delta_smelt_all_years"]] <- delta_smelt_all_years
fish_fall[["longfin_smelt_all_years"]] <- longfin_smelt_all_years
fish_fall[["striped_bass0_all_years"]] <- striped_bass0_all_years
fish_fall[["american_shad_all_years"]] <- american_shad_all_years
fish_fall[["delta_smelt_recent"]] <- delta_smelt_recent
fish_fall[["longfin_smelt_recent"]] <- longfin_smelt_recent
fish_fall[["striped_bass0_recent"]] <- striped_bass0_recent
fish_fall[["american_shad_recent"]] <- american_shad_recent
fish_fall[["white_sturgeon_all_years"]] <- white_sturgeon_all_years
fish_fall[["white_sturgeon_recent"]] <- white_sturgeon_recent

save(list="fish_fall", file=file.path(fig_root_fall,"fish_fall.RData"))

