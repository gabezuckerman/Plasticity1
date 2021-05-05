library(foreach)
library(doParallel)
library(tidyverse)
library(data.table)
library(lubridate)
library(irg)
library(gtools)

# setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Switching2/covariateData/MOD09Q1/")
# gps <- fread("gpsNDVIts.csv") %>% distinct(gps_sensors_animals_id, acquisition_time, .keep_all = TRUE)



gps <- fread("gpsNDVIts.csv")

gps$acquisition_time <- ymd_hms(gps$acquisition_time)

gps$row <- seq(1, nrow(gps))



#calculates irg and vegetation biomass for every single point
#(ndvi amplitude = height relative to baseline (set as lower 2.5th percentile), ie smoothed, scaled ndvi)
#takes in one row from gpsNDVIts
getIRGandBiomassRow <- function(rowNum) {
  
  rowInDF <- gps %>% filter(row == rowNum)
  
  #only completing for spring migration and summer periodsa
  if (rowInDF$period %in% c("winter1", "fallMig", "winter2")) {
    returnDF <- rowInDF[, 1:10]
    returnDF$dfp <- NA
    returnDF$irg <- NA
    return(returnDF)
  }
  
  #removing rows where all ndvi values are NA
  ndviVals <- rowInDF[, 11:(ncol(gps)-1)]
  ndviVals <- ndviVals[rowSums(is.na(ndviVals)) != ncol(ndviVals), ]
  
  #some dates have no NDVI values
  if (nrow(ndviVals) == 0) {
    returnDF <- rowInDF[, 1:10]
    returnDF$dfp <- NA
    returnDF$irg <- NA
    return(returnDF)
  }
  
  #getting NDVI values into a column
  ts <- data.table(t(ndviVals))
  colnames(ts) <- "NDVI"
  
  #multiplying out to preserve decimels, bc irg converts to integer
  ts$NDVI <- as.numeric(ts$NDVI) * 1000000000
  
  #adding doy 
  ts$DayOfYear <- as.numeric(str_sub(colnames(ndviVals), -3))
  
  #adding id for irg sake
  ts$id <- 1
  
  #adding yr for irg sake
  ts$yr <- rowInDF$dateYear[1]
  
  #just setting to 0 bc already filtered for QA and 0 acceptable
  ts$SummaryQA <- 0
  
  #this filters ndvi according to Bischof et al
  ts <- filter_ndvi(ts)
  
  #scaling doy and ndvi to be between 0 and 1
  ts <- scale_doy(ts)
  ts <- scale_ndvi(ts)
  
  # Guess starting parameters
  ts <- model_start(ts, id = 'id', year = 'yr')
  
  # Double logistic model parameters given starting parameters for nls
  mods <- model_params(
    ts,
    returns = 'models',
    id = 'id', year = 'yr',
    xmidS = 'xmidS_start', xmidA = 'xmidA_start',
    scalS = 0.05,
    scalA = 0.01
  )
  
  #handles weird case where unable to fit double logistic model
  if(ncol(mods) == 2) {
    returnDF <- rowInDF[1:10]
    returnDF$dfp <- NA
    returnDF$irg <- NA
    return(returnDF)
  }
  
  fit <- model_ndvi(mods, observed = FALSE)
  
  #now calculating irg for each day of the year
  irg <- calc_irg(fit)
  
  
  #just doy and irg in this table
  final <- irg %>% dplyr::select(yr, t, irg) 
  
  #lubridate handles leap years
  start <- ymd(paste0(final$yr[1], "-01-01"))
  end <- ymd(paste0(final$yr[1], "-12-31"))
  
  
  #if year is leap year leaving all rows (366), otherwise dropping last row
  if(final$yr[1] %% 4 == 0) {
    final$date <- seq(start, end, by = "1 day")
  } else {
    final <- final[1:365, ]
    final$date <- seq(start, end, by = "1 day")
  }
  
  peak <- final$date[which(final$irg == max(final$irg))]
  
  final$dfp <- as.numeric(abs(final$date - peak))
  
  #joining original movement data table with irg/dfp data from calculation
  return(cbind(rowInDF[,1:10], final %>% 
                 filter(date == date(rowInDF$acquisition_time)) %>% dplyr::select(dfp, irg)))
  
}


#gets IRG and Biomass for one individual
getIRGandBiomassInd <- function(sid) {
  indRows <- filter(gps, switchID == sid)$row
  return(map_dfr(indRows, getIRGandBiomassRow))
}
##parallelizing calculation
ncores <- as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))
registerDoParallel(ncores)


all <- foreach(i = unique(gps$switchID),
               .errorhandling = 'pass',
               .packages = c('tidyverse', 'lubridate', 'irg',
                             'data.table')) %dopar% getIRGandBiomassInd(i)

fwrite(rbindlist(all), "gpsIRG.csv")

