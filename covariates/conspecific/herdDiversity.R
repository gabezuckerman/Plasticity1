library(data.table)
library(tidyverse)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Plasticity1/movementData")

distChanges <- fread("distanceChanges.csv")

#adding herd to labeledGPS
labeledGPS <- fread("labeledGPS.csv") %>% merge(
  fread("../../Switching3/movementData/multiburstsNC.csv") %>%
    distinct(gps_sensors_animals_id, herd)
)

#adding herd to dist changes
distChanges <- distChanges %>% merge(labeledGPS %>% distinct(id = switchID, herd))

#adding hypotenuse, and finding sd of for herd level distribution

distChanges %>% mutate(hyp = sqrt(abs(distanceDiff)^2 + abs(elevDiff)^2)) %>%
  group_by(herd) %>% summarise(herdSD = sd(hyp)) %>% fwrite("../covariates/conspecific/herdSD.csv")


