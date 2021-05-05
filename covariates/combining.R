library(data.table)
library(tidyverse)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Plasticity1")

distChanges <- fread("movementData/distanceChanges.csv")

labeledGPS <- fread("movementData/labeledGPS.csv") %>% merge(
  fread("../Switching3/movementData/multiburstsNC.csv") %>%
    distinct(gps_sensors_animals_id, herd)
)

#adding herd to dist changes
distChanges <- distChanges %>% merge(labeledGPS %>% distinct(id = switchID, herd)) %>%
  mutate(hyp = sqrt(abs(distanceDiff)^2 + abs(elevDiff)^2))

######################################################
cons <- fread("covariates/conspecific/herdSD.csv") %>%
  rename(herdDiversity = herdSD)

distChanges <- distChanges %>% merge(cons, by = "herd")

################################################
crop <- fread("covariates/crop/cropProp.csv") %>%
  dplyr::select(switchID, propCrop)


distChanges <- distChanges %>% rename(switchID = id) %>% merge(crop, by = "switchID")
###############################################
dev <- fread("covariates/development/devProp.csv")

dev <-dev %>% filter(period == "springMig") %>% dplyr::select(switchID, migDev = propDev) %>% 
  merge(
    dev %>% filter(period == "winter2") %>% dplyr::select(switchID, winterDev = propDev)
  )



distChanges <- distChanges %>% merge(dev, by = "switchID")


###############################################
feed <- fread("covariates/feed/feedAccess.csv") %>% dplyr::select(switchID, feedAccess)

distChanges <- distChanges %>% merge(feed)


##############################################
greenUp <- fread("covariates/greenUp/irg.csv")

springMigGreenUp <- greenUp %>% filter(period == "springMig") %>% dplyr::select(switchID, meanDFP)


distChanges <- distChanges %>% merge(springMigGreenUp)
#################################
private <- fread("covariates/private/privateProp.csv") %>% dplyr::select(switchID, propPrivate)

distChanges <- distChanges %>% merge(private)


####################################


snow <- fread("covariates/snow/snodasExtraction.csv") %>% filter(period == "springMig") %>%
  group_by(switchID) %>%  dplyr::summarise(maxSnow = max(Snow_Depth_mean, na.rm = T))

distChanges <- distChanges %>% merge(snow)

fwrite(distChanges, "movementData/distanceChangesWithCov.csv")
