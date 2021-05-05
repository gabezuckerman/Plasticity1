library(data.table)
library(tidyverse)
library(raster)
library(velox)
library(lubridate)
library(tictoc)
library(doParallel)
library(furrr)
library(sf)

setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Plasticity1/covariates/private/data")

#only need 2005 onwards
lcFiles <- list.files("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Switching2/covariateData/LCMAP",
                      pattern = ".tif", full.names = T)[7:19]

private <- raster("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Switching2/covariateData/landOwnership/pvtProClip.tif")

#changing working directory to save updated rasters

extractPrivateNonDevNonCrop <- function(rasterNum) {
  tic()
  year <- str_split(lcFiles[rasterNum], "_")[[1]][2] %>% parse_number()
  
  print(year)
  
  r <- raster(lcFiles[rasterNum])
  
  print("reclassifying")
  
  #marking developed and croplands as 0, everything else as 1
  reclass <- c(0, 2.00001, 0,
               2.00001, 10, 1)
  rclmat <- matrix(reclass, ncol = 3, byrow = T)
  r_nonCropDev <- reclassify(r, rclmat, include.lowest = T, right = NA)
  
  print("masking")
  #masking to get private lands where it is neither crop nor developed
  r_privateNonCropDev <- mask(r_nonCropDev, private, updatevalue = 0)
  
  print("writing")
  #saving
  writeRaster(r_privateNonCropDev, 
              paste0("privateNonCropDevLCMAP", year, ".tif"), overwrite = T)
  toc()
}

#tested on the first one
map(2:13, extractPrivateNonDevNonCrop)
