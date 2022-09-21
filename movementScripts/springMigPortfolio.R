library(tidyverse)
library(lubridate)
library(Hmisc)
library(sf)
library(BBMM)
library(raster)
library(data.table)
library(tripack)

setwd("~/Documents/GitHub/Plasticity1/Switching3")

multibursts <- fread("movementData/multiburstsNC.csv")
multibursts$acquisition_time <- ymd_hms(multibursts$acquisition_time)

timing <- fread("timingData/updatedTiming - completed.csv", na.strings = "")

timing$elk <- as.numeric(map(timing$elkYear, ~str_split(.x, "_")[[1]][1]))
timing$year <- as.numeric(map(timing$elkYear, ~str_split(.x, "_")[[1]][2]))

#removing inds with non-consecutive years
timing <- timing %>% filter(elk %nin% c(731, 732, 754))

#finding timing for each each elk year

#contains all relevant dates, and elk year, as well as year 1 strategy, year 2 strategy
getTiming <- function(ey) {

  indTiming <- timing %>% filter(elkYear == ey)
  startYear <- filter(multibursts, elkYear == ey)$startDateYear[1]
  herd <- filter(multibursts, elkYear == ey)$herd[1]
    
  if (sum(is.na(indTiming[,4:7])) != 4) {
    #start year is always december start
    spStart <- ymd(paste0(startYear +1, "-", indTiming$manualSpStart))
    spEnd <- ymd(paste0(startYear +1, "-", indTiming$manualSpEnd))
    
    #need to account for fall migrations that start or end after december
    #checking to see if there is a fall mig
    if(!is.na(indTiming$manualFaStart)) {
      #start
      if(substr(indTiming$manualFaStart, 1, 2) %in% 
         c("1-", "2-", "3-", "01", "02", "03")) {
        faStart <- ymd(paste0(startYear +2, "-", indTiming$manualFaStart))
      } else {
        faStart <- ymd(paste0(startYear +1, "-", indTiming$manualFaStart))
      }
      #end
      if(substr(indTiming$manualFaEnd, 1, 2) %in% 
         c("1-", "2-", "3-", "01", "02", "03")) {
        faEnd <- ymd(paste0(startYear +2, "-", indTiming$manualFaEnd))
      } else {
        faEnd <- ymd(paste0(startYear +1, "-", indTiming$manualFaEnd))
      }
    } else {
      faStart <- NA
      faEnd <- NA
    }
  } else {
    faStart <- NA
    faEnd <- NA
    spStart <- NA
    spEnd <- NA
  }
  
  
  #joining it all together
  returnTable <- data.table(elkYear = ey, elk = str_split(ey, "_")[[1]][1],
                            herd, decStartYear = startYear,
                              spStart = as.character(spStart), 
                              spEnd = as.character(spEnd),
                              faStart = as.character(faStart),
                              faEnd = as.character(faEnd))

  
  returnTable
}

allTiming <- map_dfr(unique(timing$elkYear), getTiming)


#finding herd level average timings in yday, to serve as default fill ins

getAverageHerdTiming <- function(h) {
  ht <- allTiming %>% filter(herd == h)
  
  spStart <- floor(median(yday(ht$spStart), na.rm = T))
  spEnd <- ceiling(median(yday(ht$spEnd), na.rm = T))
  faStart <- floor(median(yday(ht$faStart), na.rm = T))
  faEnd <- ceiling(median(yday(ht$faEnd), na.rm = T))

  return(data.table(herd = h, spStart, spEnd, faStart, faEnd))
}

herdAvg <- map_dfr(unique(allTiming$herd), getAverageHerdTiming)

#getting spring migration time frame for each elk year
#using herd timing if is resident
labelSpringGPSdata <- function(ey) {
  ind <- allTiming %>% filter(elkYear == ey)
  herdTiming <- herdAvg %>% filter(herd == ind$herd)
  
  spStart <- if_else(!is.na(ind$spStart), ind$spStart, 
                     as.character(ymd(paste0(ind$decStartYear + 1, "-1-1")) + herdTiming$spStart))
  spEnd <- if_else(!is.na(ind$spEnd), ind$spEnd, 
                   as.character(ymd(paste0(ind$decStartYear + 1, "-1-1")) + herdTiming$spEnd))
  
  
  springMig <- interval(ymd(spStart) - days(1), ymd(spEnd) + days(1))
  
  
  springMigData <- multibursts %>% filter(gps_sensors_animals_id == ind$elk,
                                          acquisition_time %within% springMig) %>%
    mutate(period = "springMig") %>%
    dplyr::select(elkYear,  elk = gps_sensors_animals_id,period, startDateYear,
           acquisition_time, longitude, latitude, elevation)
  
  springMigData
  
}


library(doSNOW)
cl <- makeCluster(12)
registerDoSNOW(cl)

pb <- txtProgressBar(max = nrow(allTiming), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


springLabeled <- foreach(i = allTiming$elkYear,
                  .errorhandling = 'pass',
                  .options.snow = opts,
                  .combine = 'rbind',
                  .packages = c('tidyverse', 'lubridate','data.table')) %dopar%  labelSpringGPSdata(i)
stopCluster(cl)




springLabeled <- springLabeled %>%  
  cbind(springLabeled %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
          st_transform(32612) %>% st_coordinates())


#to get elevation percetiles
range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

springLabeled <- springLabeled %>% filter(startDateYear > 2003)

#getting spring mig length and elevation change
getCorridorLength <- function(ey) {
  data <- springLabeled %>% filter(elkYear == ey) %>%
    dplyr::select(elk, startDateYear, acquisition_time, X, Y, elevation) %>% arrange(acquisition_time)
  data$elevPercentile <- range01(data$elevation)
  
  #elevation change is 97.5% elev - 2.5% elev during spring mig
  
  two.5 <- max(filter(data, elevPercentile <= .025)$elevation, na.rm = T)
  ninety7.5 <- ifelse(nrow(filter(data, elevPercentile >= .9)) == 0, 
                      max(data$elevation, na.rm = T),
                      min(filter(data, elevPercentile >= .9)$elevation, na.rm = T)) 
  
  #determining which came first to see if elevation was change was up or down
  
  if(which(data$elevation == two.5) > which(data$elevation == ninety7.5)){
    elevationChange <- two.5 - ninety7.5
  } else {
    elevationChange <- ninety7.5 - two.5
  }
  
  #fitting bbmm
  timeLag <- as.numeric(difftime(data$acquisition_time, lag(data$acquisition_time), 
                                 units = "mins"))
  #removing either the top or bottom 1% of timelags
  timeLagPercentiles <- range01(timeLag)
  
  time <- data.table(timeLag, timeLagPercentiles)
  
  data <- cbind(data, time) %>% arrange(acquisition_time)
  
  #removing the top 1% of fixes, unless all the data are in the top 1%, then remove bottom 1%
  numInTop1 <- filter(data, timeLagPercentiles >= 0.99) %>% nrow()
  bottom99 <- filter(data, timeLagPercentiles < 0.99) %>% nrow()
  if (length(unique(data$timeLag[2:nrow(data)])) != 1) {
    #checking to see if there is more data in the top 1% then the rest
    if (numInTop1 > bottom99) {
      data <- data[-which(data$timeLagPercentiles < .01),]
    } else {
      data <- data[-which(data$timeLagPercentiles > .99),]
    }
  }
  
  # if (length(unique(data$timeLag[2:nrow(data)])) != 1) {
  #   #if more are in the 99th percentile, then removing the top 1%
  #   if(length(which(data$timeLagPercentiles > .99)) > length(which(data$timeLagPercentiles < .01))){
  #     data <- data[-which(data$timeLagPercentiles > .99),]
  #   } else {
  #     data <- data[-which(data$timeLagPercentiles < .01),]
  #     
  #     
  #   }
  # }
  
  #ought to be 25 meters to get all to fit correctly, but takes too long
  
  bb <- brownian.bridge(x = data$X, y = data$Y, time.lag = data$timeLag[2:nrow(data)], 
                        location.error = 30, cell.size = 100)
  
  #getting 99 contour for corridors
  contours <- bbmm.contour(bb, levels=99, locations=data, plot = T)
  bbDF <- data.table(X = bb[["x"]], Y = bb[["y"]], prob = bb[["probability"]]) 
  r <-  rasterFromXYZ(bbDF,crs=CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"),digits=2)
  #converting contour to points
  point.contour <- rasterToContour(r,levels=contours$Z) %>% st_as_sf() %>% st_cast("POINT") %>%
    st_coordinates() %>% as.data.table()
  
  #drawing circle around 99% isoplet
  circle <- circumcircle(point.contour$X, point.contour$Y, plot = F)
  #diameter of circle
  len <-  2*circle[[3]]
  
  
  return(data.table(elkYear = ey, elk = data$elk[1], decStartYear = data$startDateYear[1], len, elevationChange))
}

cl <- makeCluster(12)
registerDoSNOW(cl)

pb <- txtProgressBar(max = length(unique(springLabeled$elkYear)), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


springMigLenList <- foreach(i = unique(springLabeled$elkYear),
                         .errorhandling = 'pass',
                         .options.snow = opts,
                         .packages = c('tidyverse', 'lubridate',
                                       'data.table', 'tripack',
                                       'BBMM', 'raster', 'sf')) %dopar%  getCorridorLength(i)
stopCluster(cl)

#removing first year of the one bad year
springMigLen <- rbindlist(Filter(is.data.frame, springMigLenList)) %>% filter(len > 1)


#for each switch opp finding distance between (y1len, y1elChange) and (y2len, y2elChange)
getDistChange <- function(e) {
  ind <- springMigLen %>% filter(elk == e)
  
  #some are messed up
  if(nrow(ind) < 2) {
    return()
  }
  
  returnTable <- data.table()
  
  for(i in 1:(nrow(ind)-1)){
    id <- paste(ind$elkYear[i], str_split(ind$elkYear[i+1], "_")[[1]][2], sep = "-")
    
    #2nd year - 1st year
    distanceDiff <- ind$len[i+1] - ind$len[i]
    elevDiff <- ind$elevationChange[i+1] - ind$elevationChange[i]
    
    returnTable  <- rbind(returnTable, data.table(id, distanceDiff, elevDiff, 
                             sp1dist = ind$len[i], sp1elev = ind$elevationChange[i], 
                             sp2dist = ind$len[i+1], sp2elev = ind$elevationChange[i+1]))
  }
  returnTable
}

allDist <- map_dfr(unique(springMigLen$elk), getDistChange)
fwrite(allDist, "modelingScripts/take2/distanceChanges.csv")

