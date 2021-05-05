library(rgdal)
library(gdalUtils)
library(furrr)
library(raster)
library(foreach)
library(doParallel)
library(tidyverse)
library(tictoc)
library(data.table)
library(lubridate)
library(sf)
library(velox)
library(gtools)

##Extracts a timeseries of NDVI data for each point for every image in the given year


setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Switching3/covariates/greenUp")

gps <- fread("../../movementData/labeledGPS.csv") 
gps$acquisition_time <- ymd_hms(gps$acquisition_time)
gps$dateYear <- year(gps$acquisition_time)


#only 2006097 one has wrong extent
#so removed from dataset


#extracting values for one year of rasters at a time
getYearNDVI <- function(y){
  
  #getting movement data from given year
  toExtract <- filter(gps, dateYear == y)
  
  #converting to sf object
  toExtractSF <- toExtract %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  #stacking one year of rasters
  ndvi <- stack(map(list.files("data/cleanedNDVI/", pattern = paste0("QC", as.character(y)),
                               full.names = T), raster))

  #converting to velox
  vNDVI <- velox(ndvi)
  
  #extracting values from year of rasters
  vals <- vNDVI$extract_points(toExtractSF)
  
  #getting day of year as column names
  fileNames <- list.files("data/cleanedNDVI/", pattern = paste0("QC", as.character(y)))
  colnames(vals) <- paste0("NDVI", str_sub(str_extract(fileNames, "[0-9]+"), -3))
  
  
  #appending NDVI to movement data
  all <- cbind(toExtract, vals)
  
  return(all)
}




##parallelizing calculation
cl <- makeCluster(6)

registerDoParallel(cl)

tic()
allYears <- foreach(y = unique(gps$dateYear),
                     .errorhandling = 'stop',
                     .packages = c('tidyverse', 'lubridate', 'sf',
                                   'data.table', 'velox',
                                   'rgeos', 'geosphere', 
                                   'raster', 'gdalUtils')) %dopar% getYearNDVI(y)
toc()
stopCluster(cl)

allYears2 <- rbindlist(allYears, fill = T)
allYears2$acquisition_time <- ymd_hms(allYears2$acquisition_time)
allYears2 <- allYears2 %>% arrange(acquisition_time)
fwrite(allYears2, "data/gpsNDVIts.csv")
