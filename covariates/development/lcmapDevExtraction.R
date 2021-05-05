library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(lubridate)
library(foreach)
library(doParallel)
library(readr)
library(tictoc)


setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Plasticity1")

#dropping earlier years because of there is no snow data
rangeCorridors <- st_read("movementData/springWinter2", "all") %>% filter(year > 2003)
st_crs(rangeCorridors) <- 32612


devRasters <- list.files("covariates/development//data", full.names = T)[8:19]


#crop is all periods except summer
getDevProp <- function(dr) {
  
  print(parse_number(dr))
  
  #max year is 2017
  if(parse_number(dr) == 2017) {
    shapes <- rangeCorridors %>% filter(year >= 2017) %>%
      st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
  } else {
    shapes <- rangeCorridors %>% filter(year == parse_number(dr)) %>%
      st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
  }
  
  dev <- raster(dr)
  
  #1 is dev, 0 is not
  #finds all cells that polygon covers then returns proportion that are private
  getProp <- function(row) {
    ind <- raster::extract(dev, shapes[row,])
    sum(as.numeric(do.call(c, ind)) == 1) / length(as.numeric(do.call(c, ind))) %>% return()
  }
  
  returnDF <- shapes %>% st_drop_geometry()
  returnDF$propDev <- map_dbl(seq(1, nrow(shapes)), getProp)
  
  return(returnDF)  
}


allYears <- map(devRasters, getDevProp)
rbindlist(allYears) %>% fwrite("covariates/development/devProp.csv")
