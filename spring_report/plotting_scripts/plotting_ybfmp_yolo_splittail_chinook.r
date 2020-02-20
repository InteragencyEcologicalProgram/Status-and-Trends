library(tidyverse)
library(lubridate)
library(smonitr)

report_year = 2018
fig.root = file.path("spring_report", "figures")


# Path to sharepoint site
sharepoint_path = normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - Seasonal monitoring report - Documents/Data"
  )
)
# save data
load(file.path(sharepoint_path, "ybfmp.RData"))


catch = ybfmp[["YBFMP_Fish_Catch_and_Water_Quality.csv"]] %>%
  mutate(
    SampleDate = mdy(SampleDate),
    Month = as.integer(month(SampleDate)),
    Year = as.integer(year(SampleDate))
  )

effort = ybfmp[["YBFMP_Trap_Effort.csv"]] %>%
  mutate(
    Year = as.integer(Year),
    Month = as.integer(factor(Month, month.abb))
  )

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

yolo_splittail_fig = cpue %>%
  filter(CommonName == "Splittail") %>% {
  ggplot(., aes(x = Year, y = CPUE)) +
    theme_smr() +
    geom_col(fill = "black") +
    lt_avg_line(lt_avg = mean(.$CPUE, na.rm = TRUE)) +
    std_x_axis_all_years(rpt_yr = report_year,
      start_yr = min(.$Year)) +
    scale_y_continuous("Fish catch per thousand hours",
      labels = scales::label_number(scale = 1000))
  }

yolo_chinook_fig = cpue %>%
  filter(CommonName == "Chinook Salmon") %>% {
    ggplot(., aes(x = Year, y = CPUE)) +
    theme_smr() +
    geom_col(fill = "black") +
    lt_avg_line(lt_avg = mean(.$CPUE, na.rm = TRUE)) +
    std_x_axis_all_years(rpt_yr = report_year,
      start_yr = min(.$Year)) +
    scale_y_continuous("Fish catch per thousand hours",
      labels = scales::label_number(scale = 1000))
  }

ggsave(yolo_splittail_fig, file = file.path(fig.root, "yolo_splittail.png"),
  dpi = 300, units = "cm", width = 9.3, height = 6.8)

ggsave(yolo_chinook_fig, file = file.path(fig.root, "yolo_chinook.png"),
  dpi = 300, units = "cm", width = 9.3, height = 6.8)
