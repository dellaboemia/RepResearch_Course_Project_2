# Coursera Data Science Specialization
# Reproducible Research
# Course Project 2
# Author: John James
# Date: March 7, 2016
# master.R


#############################################################################
##                            ENVIRONMENT                                  ##
#############################################################################
## ---- environment

# Processes to run
  load    <- TRUE
  review  <- TRUE
  process <- TRUE
  analyze <- TRUE
  present <- FALSE
 
# Raw Data File Parameters
  dataUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"   # Link to data source
  dataZipFile     <- "stormData.zip"    # Name of zip file (get this from the data source code book)
  dataCsvFile     <- "stormData.csv"    # Name of csv file (get this from the data source code book)
  dataFrame       <- "stormData"        # Name of data frame that will contain the storm data

# Directories
  dataRawDir  <- "Data/Raw Data/"       # Directory for raw data    
  dataProcDir <- "Data/Processed Data/" # Directory for processed data
  codeDir     <- "Code/"                # Directory for code
  figDir      <- "Figure/"              # Directory for Figures

# Load custom functions files
source(file=paste(codeDir, "load.R", sep=""))
source(file=paste(codeDir, "review.R", sep=""))
source(file=paste(codeDir, "process.R", sep=""))
source(file=paste(codeDir, "analyze.R", sep=""))
source(file=paste(codeDir, "present.R", sep=""))
  
# Include requisite libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(downloader)

## ---- end environment

#############################################################################
##                                LOAD DATA                                ##
#############################################################################
## ---- loadDataCall

    if (load) {
    stormData <- loadData(dataUrl, dataRawDir, dataZipFile, dataCsvFile, dataFrame)

}
## ---- end
  
  
  
#############################################################################
##                              REVIEW DATA                                ##
#############################################################################
## ---- reviewData
  if (review) {

  # Call review data function  
  dataReport <- reviewData(stormData)

  # Ascertain dimensions of dataset
  observations  <- dataReport[[1]][1]
  variables     <- dataReport[[1]][2]

  # Determine number of unique event types
  numEventTypes <- dataReport[[2]][1]

  # Obtain number of zero rows
  zeroRows <- dataReport[[3]][1]

}  
## ---- end
  
  
#############################################################################
##                              PROCESS DATA                               ##
#############################################################################

  if (process) {
    
## ---- removeZeroRows 
    stormData <- stormData[ which(stormData$FATALITIES != 0 | stormData$INJURIES != 0 | stormData$PROPDMG != 0 | stormData$CROPDMG != 0),]
## ---- end
    
## ---- extractVariables
    stormData <- select(stormData, BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
## ---- end
    
## ---- processEventTypeCall
    stormData <- processEventType(stormData)
## ---- end
    
## ---- processDamageCall
    stormData <- processDamage(stormData)
## ---- end
    
## ---- processYearCall
    stormData <- processYear(stormData)
## ---- end
      
}
# ---- endProcess  



#############################################################################
##                              ANALYZE DATA                               ##
#############################################################################
  if (analyze) {

## ---- top5AllData
    top5Fatalities  <- top5FatalitiesFunc(stormData)
    top5Injuries    <- top5InjuriesFunc(stormData)
    top5Health      <- top5HealthFunc(stormData)
    top5CropDmg     <- top5CropDmgFunc(stormData)
    top5PropDmg     <- top5PropDmgFunc(stormData)
    top5TotalDmg    <- top5TotalDmgFunc(stormData)
## ---- end
    
## ---- extract1980Data
    stormData1980 <- stormData[ which(stormData$YEAR >= "1980"),]
## ---- end
    
## ---- top51980
    top5Fatalities1980  <- top5FatalitiesFunc(stormData1980)
    top5Injuries1980    <- top5InjuriesFunc(stormData1980)
    top5Health1980      <- top5HealthFunc(stormData1980)
    top5CropDmg1980     <- top5CropDmgFunc(stormData1980)
    top5PropDmg1980     <- top5PropDmgFunc(stormData1980)
    top5TotalDmg1980    <- top5TotalDmgFunc(stormData1980)
## ---- end
  
## ---- timeSeriesData
    fatalitiesByYear  <- stormData %>%  group_by(YEAR,EVTYPE) %>% summarise(FATALITIES = sum(FATALITIES))
    fatalitiesByYear  <- fatalitiesByYear[ which(fatalitiesByYear$EVTYPE %in% top5Fatalities$EVTYPE), ]
    
    injuriesByYear    <- stormData %>%  group_by(YEAR,EVTYPE) %>% summarise(INJURIES = sum(INJURIES))
    injuriesByYear    <- injuriesByYear[ which(injuriesByYear$EVTYPE %in% top5Injuries$EVTYPE), ]
    
    healthIncidentsByYear    <- stormData %>%  group_by(YEAR,EVTYPE) %>% summarise(TOTALHEALTH = sum(TOTALHEALTH))
    healthIncidentsByYear    <- healthIncidentsByYear[ which(healthIncidentsByYear$EVTYPE %in% top5Health$EVTYPE), ]
    
    cropDmgByYear   <- stormData %>%  group_by(YEAR,EVTYPE) %>% summarise(CROPDMG = sum(CROPDMG))
    cropDmgByYear   <- cropDmgByYear[ which(cropDmgByYear$EVTYPE %in% top5CropDmg$EVTYPE), ]
    
    propDmgByYear   <- stormData %>%  group_by(YEAR,EVTYPE) %>% summarise(PROPDMG = sum(PROPDMG))
    propDmgByYear   <- propDmgByYear[ which(propDmgByYear$EVTYPE %in% top5PropDmg$EVTYPE), ]

    totalDmgByYear  <- stormData %>%  group_by(YEAR,EVTYPE) %>% summarise(TOTALDMG = sum(TOTALDMG))
    totalDmgByYear  <- totalDmgByYear[ which(totalDmgByYear$EVTYPE %in% top5TotalDmg$EVTYPE), ]
## ---- end
    
}


#############################################################################
##                              PRESENT DATA                               ##
#############################################################################
# ---- presentData  

  if (present) {
    message("Presenting data...")
    
    # Present all data
    b1 <- barPlotFatalities(top5Fatalities)
    b2 <- barPlotInjuries(top5Injuries)
    b3 <- barPlotHealthIncidents(top5Health)
    b4 <- barPlotPropertyDamage(top5PropDmg)
    b5 <- barPlotCropDamage(top5CropDmg)
    b6 <- barPlotTotalDamage(top5TotalDmg)
    
    grid.arrange(b1, b2, b3, b4, b5, b6, ncol=2, nrow=3)    

    
    # Present data since 1980
    b1 <- barPlotFatalities(top5Fatalities1980)
    b2 <- barPlotInjuries(top5Injuries1980)
    b3 <- barPlotHealthIncidents(top5Health1980)
    b4 <- barPlotPropertyDamage(top5PropDmg1980)
    b5 <- barPlotCropDamage(top5CropDmg1980)
    b6 <- barPlotTotalDamage(top5TotalDmg1980)
    
    grid.arrange(b1, b2, b3, b4, b5, b6, ncol=2, nrow=3)    
    

    # Present data since 2000
    b1 <- barPlotFatalities(top5Fatalities2000)
    b2 <- barPlotInjuries(top5Injuries2000)
    b3 <- barPlotHealthIncidents(top5Health2000)
    b4 <- barPlotPropertyDamage(top5PropDmg2000)
    b5 <- barPlotCropDamage(top5CropDmg2000)
    b6 <- barPlotTotalDamage(top5TotalDmg2000)
    
    grid.arrange(b1, b2, b3, b4, b5, b6, ncol=2, nrow=3)    
    

    # Present trend lines
    t1 <- trendFatalities(fatalitiesByYear)
    t2 <- trendInjuries(injuriesByYear)
    t3 <- trendHealth(healthIncidentsByYear)
    t4 <- trendPropDmg(propDmgByYear)
    t5 <- trendCropDmg(cropDmgByYear)
    t6 <- trendTotalDmg(totalDmgByYear)
    print(t1)
    print(t2)
    print(t3)
    print(t4)
    print(t5)
    print(t6)

    grid.arrange(t1, t2, t3, t4, t5, t6, ncol=2, nrow=3)    
    
}

# ---- end presentData
  
    
