# Coursera Data Science Specialization
# Reproducible Research
# Course Project 2
# Author: John James
# Date: March 7, 2016
# master.R


#############################################################################
##                            ENVIRONMENT                                  ##
#############################################################################
# ---- environment

# Processes to run
  load    <- TRUE
  review  <- TRUE
  process <- TRUE
  analyze <- TRUE
  present <- TRUE
 
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

# ---- end environment

#############################################################################
##                                LOAD DATA                                ##
#############################################################################
# ---- loadData

    if (load) {
    stormData <- loadData(dataUrl, dataRawDir, dataZipFile, dataCsvFile, dataFrame)

# ---- end loadData
}

  
#############################################################################
##                              REVIEW DATA                                ##
#############################################################################
# ---- review  
  if (review) {
  # ---- reviewData
  dataReport <- reviewData(stormData)
  # ---- end reviewData  
  
  # ---- printDim
  print(dataReport[1])
  # ---- end
  
  # ---- saveEVentTypes
  eventTypes <- data.frame(dataReport[2])
  # ---- end
  
  # ---- summarizeFatalities
  print(dataReport[3])
  # ---- end
  
  # ---- summarizeInjuries
  print(dataReport[4])
  # ---- end
  
  # ---- summarizePropDmg
  print(dataReport[5])
  propDmgExp <- data.frame(dataReport[6])
  # ---- end

  # ---- summarizeCropDmg
  print(dataReport[7])
  cropDmgExp <- data.frame(dataReport[8])
  # ---- end
  
  }  
# ---- end  
#############################################################################
##                              PROCESS DATA                               ##
#############################################################################
# ---- process

  if (process) {
# Extract the columns of interest from raw data frame
  stormData <- select(stormData, BGN_DATE, LATITUDE, LONGITUDE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
  
# Process Event Types
  stormData <- processEventType(stormData)

# Process damage estimates
  stormData <- processDamage(stormData)

# Process year
  stormData <- processYear(stormData)
  
}
# ---- endProcess  



#############################################################################
##                              ANALYZE DATA                               ##
#############################################################################
# ---- analyzeData
  if (analyze) {
    message("Analyzing data...")
    
    # Analyze all data 
    top5Fatalities  <- top5FatalitiesFunc(stormData)
    top5Injuries    <- top5InjuriesFunc(stormData)
    top5Health      <- top5HealthFunc(stormData)
    top5CropDmg     <- top5CropDmgFunc(stormData)
    top5PropDmg     <- top5PropDmgFunc(stormData)
    top5TotalDmg    <- top5TotalDmgFunc(stormData)
    
    # Analyze data since 1980
    stormData1980 <- stormData[ which(stormData$YEAR >= "1980"),]
    
    # Analyze data since 1980
    top5Fatalities1980  <- top5FatalitiesFunc(stormData1980)
    top5Injuries1980    <- top5InjuriesFunc(stormData1980)
    top5Health1980      <- top5HealthFunc(stormData1980)
    top5CropDmg1980     <- top5CropDmgFunc(stormData1980)
    top5PropDmg1980     <- top5PropDmgFunc(stormData1980)
    top5TotalDmg1980    <- top5TotalDmgFunc(stormData1980)
    
    
    # Analyze data since 2000
    stormData2000 <- stormData[ which(stormData$YEAR >= "2000"),]
    
    # Analyze data since 2000
    top5Fatalities2000  <- top5FatalitiesFunc(stormData2000)
    top5Injuries2000    <- top5InjuriesFunc(stormData2000)
    top5Health2000      <- top5HealthFunc(stormData2000)
    top5CropDmg2000     <- top5CropDmgFunc(stormData2000)
    top5PropDmg2000     <- top5PropDmgFunc(stormData2000)
    top5TotalDmg2000    <- top5TotalDmgFunc(stormData2000)
    
    # Annual summary data
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
    
}

# ---- end analyzeData
  
  

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
  
    
