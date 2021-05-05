library(rgdal)
library(gdalUtils)
library(furrr)
library(raster)
library(foreach)
library(doParallel)
library(tidyverse)
library(tictoc)
library(spatialEco)
library(data.table)
library(lubridate)
library(sf)


setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Switching3/covariates/greenUp/data/appears2020/")

#all in the same order so first band 1 goes with first band 2 with first qc band
band1files <- list.files(pattern=".*_b01") %>% sort()
band2files <- list.files(pattern=".*_b02") %>% sort()
bandQCfiles <- list.files(pattern=".*_qc") %>% sort()


#function to calculate ndvi
ndvi <- function(nirBand, redBand) {
  numerator <- nirBand - redBand
  denominator <- nirBand + redBand
  return(numerator/denominator)
}


#returns integer value of first two bits
#returns 0 if bit mask is NA
#if integer value is 0, 1 then usable so returns 1
#if integer value is 2, 3 then not usable so returns 0
getIntFromQC <- function(val) {
  #converts value's first to bits to numeric vector
  vec <- as.numeric(strsplit(parse.bits(val, bit = c(0, 1)), " ")[[1]])
  powers.of.two <- 2^(0:(1))
  qcInt <- as.integer(vec %*% powers.of.two)
  if(qcInt %in% c(0, 1)) {
    return(1)
  }
  return(0)
}






#reading in movement data and transforming it to NDVI proj
#and converting to spatial object to mask raster

gps <- fread("../../../../movementData/labeledGPS.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs") %>%
  as_Spatial()


cleanNDVI <- function(fileNum) {
  
  print(fileNum)
  
  #getting date from file name
  ydoy <- gsub("[^0-9]", "",  strsplit(band1files[fileNum], "_")[[1]][5])
  
  #getting day of year
  doy <- as.numeric(str_sub(ydoy, -3))
  
  # #not performing caculation if not during growing season
  # if (doy < 250) {
  #   print("already computed")
  #   return()
  # }
  
  #reading in bands
  nir <- readAll(raster(band2files[fileNum]))
  red <- readAll(raster(band1files[fileNum]))
  
  #removing erroneous values from ndvi layer
  ndviReclass <- c(-Inf, -1.000001, NA,
                   1.0000001, Inf, NA)
  ndviRclmat <- matrix(ndviReclass, ncol = 3, byrow = T)
  
  #creating ndvi image
  ndviIm <- reclassify(overlay(nir, red, fun = ndvi), ndviRclmat, include.lowest = T, right = NA)
  
  #removing one band images
  rm(nir)
  rm(red)
  
  #masking to only have values at pixels for which there are movement data in
  #then trimming to not include extra (NA values)
  ndviImMT <- trim(mask(ndviIm, gps))
  
  #removing non trimmed raster
  rm(ndviIm)
  
  #reading in quality raster, then masking and trimming
  qc <- readAll(raster(bandQCfiles[fileNum]))
  qcMT <- trim(mask(qc, gps))

  #removing untrimmed qc
  rm(qc)
  
  #converting qc to represent integer value quality flag from first 2 bits
  #0 and 1 are usable, 2 and 3 not
  #this is slowest part of function
  qcInt <- calc(qcMT, fun = getIntFromQC)

  #removing non-calculated qc
  rm(qcMT)
  
  #then masking to only take usable values
  #since mask value is 0, converts cells that are 0 in qcInt to NA in ndvi layer
  ndviQC <- mask(ndviImMT, qcInt, maskvalue = 0)
  
  #removing qc and non-masked ndvi images
  rm(qcInt)
  rm(ndviImMT)
  
  
  #reprojecting to 4326 and saving new raster layer
  ndviQC <- projectRaster(ndviQC, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                          filename = paste0("../cleanedNDVI/", "ndviQC", ydoy, ".tif"))
  
  #removing remaining objects
  rm(ndviQC)
}

#missing 41, 153, 177, 249, 297, 313, 321
missing <- c(7, 21, 24, 34, 40, 42, 43)

##parallelizing calculation
cl <- makeCluster(6)

registerDoParallel(cl)


ndviFiles <- foreach(id = missing,
              .errorhandling = 'pass',
              .packages = c('tidyverse', 'lubridate', 'sf',
                            'data.table', 'adehabitatHR',
                            'rgeos', 'geosphere', 'raster',
                            'spatialEco', 'gdalUtils')) %dopar% cleanNDVI(id)

stopCluster(cl)


