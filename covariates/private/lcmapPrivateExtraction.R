library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(lubridate)
library(foreach)
library(doParallel)
library(readr)


setwd("C:/Users/MiddletonLab/Desktop/Gabe/Box Sync/Elk/Plasticity1")

#dropping earlier years because of there is no snow data
rangeCorridors <- st_read("movementData/springWinter2", "all") %>% filter(year > 2003)
st_crs(rangeCorridors) <- 32612

privateRasters <- list.files("covariates/private/data", full.names = T)

#private is all periods except summer
getPrivateProp <- function(pr) {
  
  print(parse_number(pr))
  
  #max year is 2017
  if(parse_number(pr) == 2017) {
    shapes <- rangeCorridors %>% filter(year >= 2017, period == "winter2") %>%
      st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
  } else {
    shapes <- rangeCorridors %>% filter(year == parse_number(pr), period == "winter2") %>%
      st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") 
  }
  
  if(nrow(shapes) == 0) {
    return()
  }
  
  private <- raster(pr)
  
  #1 is private, 0 is not
  #finds all cells that polygon covers then returns proportion that are private
  getProp <- function(row) {
    ind <- raster::extract(private, shapes[row,])
    sum(as.numeric(do.call(c, ind)) == 1) / length(as.numeric(do.call(c, ind))) %>% return()
  }
  
  returnDF <- shapes %>% st_drop_geometry()
  returnDF$propPrivate <- map_dbl(seq(1, nrow(shapes)), getProp)
  
  return(returnDF)  
}

#none in 2005, 2006
allYears <- map(privateRasters[3:13], getPrivateProp)
rbindlist(allYears) %>% fwrite("covariates/private/privateProp.csv")


