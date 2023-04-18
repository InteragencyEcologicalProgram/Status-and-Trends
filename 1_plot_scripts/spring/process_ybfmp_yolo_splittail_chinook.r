
source("setup.R")

library(lubridate)

load(file.path(data_root, "ybfmp.RData"))

# format catch data
catch = ybfmp[["Integrated Water Quality and Fish Catch"]] %>%
  mutate(
    SampleDate = mdy(SampleDate),
    Month = as.integer(month(SampleDate)),
    Year = as.integer(year(SampleDate))
  )

# format effort data
effort = ybfmp[["Historical Monthly Trap Effort"]] %>%
  mutate(Month = as.numeric(factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                        labels = c(1:12)))) %>%
  filter(Month %in% c(3,4,5), MethodCode == "RSTR") %>%
  group_by(Year) %>%
  summarize(Time = sum(OperationTimeHRS))

#old effort is in a different spot....

cpueold = catch %>%
  filter(
    MethodCode == "RSTR", 
    IEPFishCode %in% c("SPLITT", "CHISAL"),
    Month %in% c(3, 4, 5), Year < 2010,
  ) %>%
  group_by(StationCode, MethodCode, IEPFishCode, Year) %>%
  summarize(
    Catch = sum(Count, na.rm =T) ) %>%
  left_join(effort) %>%
  ungroup() %>%
  mutate( CPUE = Catch / Time)


# now the newer data
cpue = catch %>%
  filter(
    MethodCode == "RSTR", 
    IEPFishCode %in% c("SPLITT", "CHISAL"),
	Month %in% c(3, 4, 5), Year >=2010,
  ) %>%
  group_by(StationCode, MethodCode, IEPFishCode, Year) %>%
  summarize(
    Catch = sum(Count, na.rm =T), 
    Time = as.numeric(sum(TrapHours, na.rm =T))
  ) %>%
  ungroup() %>%
  mutate( CPUE = Catch / Time)

cpue = bind_rows(cpueold, cpue)


# full plots
yolo_splittail_all = cpue %>%
  filter(IEPFishCode== "SPLITT") %>% 
  dplyr::mutate(CPUE_1000 = CPUE*1000) %>% {
    ggplot(., aes(x = Year, y = CPUE_1000)) +
    smr_theme_update() + 
    geom_col() + 
      smr_x_axis(report_year, type = "all", season = "fall") +
      stat_missing(size=2.5)+
      stat_lt_avg()+
    scale_y_continuous("Fish catch per 1,000 hours") + 
	smr_caption(stat_name="Splittail CPUE", report_year=report_year) + 
	smr_alttext(stat_name="average Sacramento Splittail catch per unit effort")		
      ##labels = scales::label_number(scale = 1000))
  }


yolo_chinook_all = cpue %>%
  filter(IEPFishCode == "CHISAL") %>%
	mutate(CPUE_1000 = CPUE*1000) %>% {
    ggplot(., aes(x = Year, y = CPUE_1000)) +
      smr_theme_update() +
      geom_col() +
      smr_x_axis(report_year, type = "all", season = "spring") +
      stat_missing(size=2.5)+
      stat_lt_avg()+
      scale_y_continuous("Fish catch per 1,000 hours")
                         ##labels = scales::label_number(scale = 1000))
  }


# recent plots
yolo_splittail_recent = cpue %>%
  filter(IEPFishCode== "SPLITT") %>% 
	mutate(CPUE_1000 = CPUE*1000) %>% {
    ggplot(., aes(x = Year, y = CPUE_1000)) +
      smr_theme_update() +
      geom_col() +
      smr_x_axis(report_year, type = "recent", season = "spring") +
      stat_missing(size=2.5)+
      stat_lt_avg()+
      scale_y_continuous("Fish catch per 1,000 hours")
                         ##labels = scales::label_number(scale = 1000))
  }

yolo_chinook_recent = cpue %>%
  filter(IEPFishCode== "CHISAL") %>% 
	mutate(CPUE_1000 = CPUE*1000) %>% {
    ggplot(., aes(x = Year, y = CPUE_1000)) +
    smr_theme_update() +
      geom_col() +
      smr_x_axis(report_year, type = "recent", season = "spring") +
      stat_missing(size=2.5)+
      stat_lt_avg()+
    scale_y_continuous("Fish catch per 1,000 hours")
      ##labels = scales::label_number(scale = 1000))
  }


# save plots

yolo_splittail_all
getCaption(yolo_splittail_all)

getAlttext(yolo_splittail_all)

save(list=c("yolo_splittail_all"), 
		 file=file.path(fig_root_spring,"yolo_splittail.RData"))



# test comparison to average of months instead of total season

cpue.bymonth = catch %>%
  filter(
    MethodCode == "RSTR",
    CommonName %in% c("Splittail", "Chinook Salmon"),
  Month %in% c(3L, 4L, 5L)
  ) %>%
  left_join(effort, by = c("Year", "Month", "MethodCode")) %>%
  group_by(StationCode, MethodCode, CommonName, Year, Month) %>%
  summarize(
    Catch = sum(Count),
    Time = as.numeric(sum(OperationTimeHRS))
  ) %>%
  summarize(
    CPUE = mean(Catch / Time),
    num.obs = n()
  ) %>%
  ungroup()

left_join(
  select(cpue, Year, CommonName, CPUE),
  select(cpue.bymonth, Year, CommonName, CPUE),
  by = c("Year", "CommonName"),
  suffix = c(".total", ".mean")
) %>%
  rename(total = CPUE.total, mean = CPUE.mean) %>%
  gather(type, CPUE, mean, total) %>%
  ggplot() + aes(x = Year, y = CPUE, fill = type) +
  geom_col(position = "dodge") + facet_wrap(~CommonName)
