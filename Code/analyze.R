# Coursera Data Science Specialization
# Reproducible Research
# Course Project 2
# Author: John James
# Date: March 16, 2016
# analyze.R


#############################################################################
##                         ANALYSIS FUNCTIONS                              ##
#############################################################################

## ---- analysisFunc
top5FatalitiesFunc <- function(x) { 
  
  fatalitiesByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(FATALITIES = sum(FATALITIES))
  
  fatalitiesByEvent <- arrange(fatalitiesByEvent,desc(FATALITIES))
  
  top5Fatalities <- fatalitiesByEvent[1:5,]
  
  return(top5Fatalities)

}


top5InjuriesFunc <- function(x) {

  injuriesByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(INJURIES = sum(INJURIES))

injuriesByEvent <- arrange(injuriesByEvent,desc(INJURIES))

top5injuries <- injuriesByEvent[1:5,]

return(top5injuries)

}



top5HealthFunc <- function(x) {
  
  healthByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(TOTALHEALTH = sum(TOTALHEALTH))
  
  healthByEvent <- arrange(healthByEvent,desc(TOTALHEALTH))
  
  top5Health <- healthByEvent[1:5,]
  
  return(top5Health)
  
}


top5CropDmgFunc <- function(x) {

  cropDmgByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(CROPDMG = sum(CROPDMG))
  
  cropDmgByEvent <- arrange(cropDmgByEvent, desc(CROPDMG))  
  
  top5CropDmg <- cropDmgByEvent[1:5,]

  return(top5CropDmg)
  
}


top5PropDmgFunc <- function(x) {

  propDmgByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(PROPDMG = sum(PROPDMG))
  
  propDmgByEvent <- arrange(propDmgByEvent, desc(PROPDMG))  
  
  top5PropDmg <- propDmgByEvent[1:5,]
  
  return(top5PropDmg)

}



top5TotalDmgFunc <- function(x) {
  
  totalDmgByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(TOTALDMG = sum(TOTALDMG))
  
  totalDmgByEvent <- arrange(totalDmgByEvent, desc(TOTALDMG))  
  
  top5TotalDmg <- totalDmgByEvent[1:5,]
  
  return(top5TotalDmg)

}
## ---- end