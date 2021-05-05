library(data.table)
library(tidyverse)
library(lubridate)
library(Hmisc)
library(sf)
library(BBMM)
library(raster)

#only taking mig and winter 2 for hypothesis testing
allGPSlabeled <- fread("movementData/labeledGPS.csv" )%>% filter(period %in% c("springMig", "winter2"))

allPeriods <- allGPSlabeled %>% distinct(switchID, period) 


allGPSlabeled <- allGPSlabeled %>% cbind(
  allGPSlabeled %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(32612) %>% st_coordinates()
)

allGPSlabeled$acquisition_time <- ymd_hms(allGPSlabeled$acquisition_time)

#for calculating year of range for extraction
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#to get time lag percetiles
range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

getBBMM99isopleth <- function(row){
  #subsetting to period
  data <- allGPSlabeled %>% filter(switchID == allPeriods$switchID[row],
                                   period == allPeriods$period[row]) %>%
    dplyr::select(switchID, acquisition_time, X, Y) %>% 
    distinct(acquisition_time, .keep_all = T) %>% arrange(acquisition_time)
  
  year <- mode(year(data$acquisition_time))
  
  #fitting bbmm
  timeLag <- as.numeric(difftime(data$acquisition_time, lag(data$acquisition_time), 
                                 units = "mins"))
  #removing either the top or bottom 1% of timelags
  timeLagPercentiles <- range01(timeLag)
  
  time <- data.table(timeLag, timeLagPercentiles)
  
  data <- cbind(data, time) 
  
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
  
  #ought to be 25 meters to get all to fit correctly, but takes too long

  bb <- brownian.bridge(x = data$X, y = data$Y, time.lag = data$timeLag[2:nrow(data)], 
                        location.error = 30, cell.size = 100)
  
  
  #getting 99 contour for corridors
  contours <- bbmm.contour(bb, levels=99, locations=data, plot = F)
  bbDF <- data.table(X = bb[["x"]], Y = bb[["y"]], prob = bb[["probability"]]) 
  r <-  rasterFromXYZ(bbDF,crs=CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"),digits=2)
  out <- tryCatch(
    {
      rasterToContour(r,levels=contours$Z) %>% st_as_sf() %>% st_cast("POLYGON") %>%
        mutate(switchID = allPeriods$switchID[row], period = allPeriods$period[row], year = year) %>%
        dplyr::select(-level)
    },
    error = function(cond){
      NULL
    })
  out
}


library(doSNOW)
cl <- makeCluster(11)
registerDoSNOW(cl)

pb <- txtProgressBar(max = nrow(allPeriods), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


rangesList <- foreach(i = 1:nrow(allPeriods),
                      .errorhandling = 'pass',
                      .options.snow = opts,
                      .packages = c('tidyverse', 'lubridate','data.table',
                                    'BBMM', 'sf', 'raster')) %dopar%  getBBMM99isopleth(i)
stopCluster(cl)

ranges <- Filter(is.data.frame, rangesList) %>% mapedit:::combine_list_of_sf()

st_write(ranges, "movementData/springWinter2", "all", driver = "ESRI Shapefile")


