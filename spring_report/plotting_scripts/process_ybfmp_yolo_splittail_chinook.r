library(tidyverse)
library(lubridate)

sharepoint_path = normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - Seasonal monitoring report - Documents/Data"
  )
)

load(file.path(sharepoint_path, "ybfmp.RData"))

load("ybfmp.RData")


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

ggplot(cpue) + ggthemes::theme_few() + aes(x = Year, y = CPUE * 1000) +
  geom_col(fill = "black") + facet_wrap(~CommonName, ncol = 1) +
  scale_x_continuous(NULL) +
  ylab("fish catch per thousand hours")

ggsave("YBFMP_yolo_spittail_chinook.pdf", width = 5, height = 5)

