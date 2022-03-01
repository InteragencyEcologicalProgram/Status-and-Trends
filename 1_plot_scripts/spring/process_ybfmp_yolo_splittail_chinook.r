library(lubridate)

load(file.path(data_root, "ybfmp.RData"))

# format catch data
catch = ybfmp[["YBFMP_Fish_Catch_and_Water_Quality.csv"]] %>%
  mutate(
    SampleDate = mdy(SampleDate),
    Month = as.integer(month(SampleDate)),
    Year = as.integer(year(SampleDate))
  )

# format effort data
effort = ybfmp[["YBFMP_Trap_Effort.csv"]] %>%
  mutate(
    Year = as.integer(Year),
    Month = as.integer(factor(Month, month.abb))
  )

# compute catch per effort - total over season
cpue = catch %>%
  filter(
    MethodCode == "RSTR", 
    CommonName %in% c("Splittail", "Chinook Salmon"),
	Month %in% c(3L, 4L, 5L)
  ) %>%
  left_join(effort, by = c("Year", "Month", "MethodCode")) %>%
  group_by(StationCode, MethodCode, CommonName, Year) %>%
  summarize(
    Catch = sum(Count), 
    Time = as.numeric(sum(OperationTimeHRS))
  ) %>%
  ungroup() %>%
  mutate(CPUE = Catch / Time)



# full plots
yolo_splittail_all = cpue %>%
  filter(CommonName == "Splittail") %>% 
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
  filter(CommonName == "Chinook Salmon") %>%
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
  filter(CommonName == "Splittail") %>% 
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
  filter(CommonName == "Chinook Salmon") %>% 
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
