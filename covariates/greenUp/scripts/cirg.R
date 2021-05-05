library(data.table)
library(tidyverse)
library(lubridate)
library(Hmisc)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Switching3/covariates/greenUp")


irg <- fread("data/gpsIRG.csv")
irg$acquisition_time <- ymd_hms(irg$acquisition_time)
#has extra rows, removing those with NA dfp
irg <- irg %>% distinct(switchID, period, acquisition_time, .keep_all = T) %>%
  filter(period %in% c("springMig", "summer")) %>% filter(year(acquisition_time) > 2003) %>%
  filter(!is.na(dfp))

#adding year-yday to
irg <- irg %>% mutate(yday = yday(acquisition_time), yyday = paste0(dateYear, yday))

#extracting the three vars
results <- irg %>% group_by(switchID, period) %>% 
  summarise(meanDFP = mean(dfp, na.rm = T),varDFP = var(dfp, na.rm = T), 
            order = cor(yday, peak, method = "spearman")) %>% filter(!is.na(order))


fwrite(results, "irg.csv")
