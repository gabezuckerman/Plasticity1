library(data.table)
library(tidyverse)
library(sf)
library(lubridate)
library(tictoc)
library(foreach)
library(doParallel)
library(Hmisc)


setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Plasticity1/")

#removing bad ranges
winterRanges <- st_read("movementData/springWinter2", "all") %>% filter(period == "winter2") 
st_crs(winterRanges) <- 32612

feedgrounds <- st_read("covariates/feed/data", "mergedFeedgrounds") %>% st_transform(32612)


results <- winterRanges %>% st_drop_geometry() %>% dplyr::select(-year)

results$numFeedgrounds <- map_dbl(1:nrow(winterRanges), ~nrow(st_intersection(winterRanges[.x,], feedgrounds)))

results <- results %>% mutate(feedAccess = ifelse(numFeedgrounds > 0, 1, 0))

fwrite(results, "covariates/feed/feedAccess.csv")
