library(lubridate)

# load from RData file on sharepoint site
sharepoint_path = normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - Seasonal monitoring report - Documents/Data"
  )
)
load(file.path(sharepoint_path, "ybfmp.RData"))

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
  filter(CommonName == "Splittail") %>% {
    ggplot(., aes(x = Year, y = CPUE)) +
    theme_smr() +
    geom_col(fill = "black") +
    lt_avg_line(lt_avg = mean(.$CPUE, na.rm = TRUE)) +
    std_x_axis_all_years(rpt_yr = report_year) +
    scale_y_continuous("Fish catch per thousand hours",
      labels = scales::label_number(scale = 1000))
  }

yolo_chinook_all = cpue %>%
  filter(CommonName == "Chinook Salmon") %>% {
    ggplot(., aes(x = Year, y = CPUE)) +
    theme_smr() +
    geom_col(fill = "black") +
    lt_avg_line(lt_avg = mean(.$CPUE, na.rm = TRUE)) +
    std_x_axis_all_years(rpt_yr = report_year) +
    scale_y_continuous("Fish catch per thousand hours",
      labels = scales::label_number(scale = 1000))
  }


# recent plots
yolo_splittail_recent = cpue %>%
  filter(CommonName == "Splittail") %>% {
    ggplot(., aes(x = Year, y = CPUE)) +
    theme_smr() +
    geom_col(fill = "black") +
    lt_avg_line(lt_avg = mean(.$CPUE, na.rm = TRUE)) +
    std_x_axis_rec_years(rpt_yr = report_year) +
    scale_y_continuous("Fish catch per thousand hours",
      labels = scales::label_number(scale = 1000))
  }

yolo_chinook_recent = cpue %>%
  filter(CommonName == "Chinook Salmon") %>% {
    ggplot(., aes(x = Year, y = CPUE)) +
    theme_smr() +
    geom_col(fill = "black") +
    lt_avg_line(lt_avg = mean(.$CPUE, na.rm = TRUE)) +
    std_x_axis_rec_years(rpt_yr = report_year) +
    scale_y_continuous("Fish catch per thousand hours",
      labels = scales::label_number(scale = 1000))
  }


# save plots
ggsave(yolo_splittail_all, file = file.path(fig_root_spring, "yolo_splittail.png"),
  dpi = 300, units = "cm", width = 9.3, height = 6.8)

ggsave(yolo_chinook_all, file = file.path(fig_root_spring, "yolo_chinook.png"),
  dpi = 300, units = "cm", width = 9.3, height = 6.8)

ggsave(yolo_splittail_recent, file = file.path(fig_root_spring, "yolo_splittail_recent.png"),
  dpi = 300, units = "cm", width = 9.3, height = 6.8)

ggsave(yolo_chinook_recent, file = file.path(fig_root_spring, "yolo_chinook_recent.png"),
  dpi = 300, units = "cm", width = 9.3, height = 6.8)


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
