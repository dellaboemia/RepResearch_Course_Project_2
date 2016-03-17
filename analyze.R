# Coursera Data Science Specialization
# Reproducible Research
# Course Project 2
# Author: John James
# Date: March 16, 2016
# analyze.R


#############################################################################
##                         ANALYSIS FUNCTIONS                              ##
#############################################################################

# ---- top5FatalitiesFunc
top5FatalitiesFunc <- function(x) { 
  
  fatalitiesByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(FATALITIES = sum(FATALITIES))
  
  fatalitiesByEvent <- arrange(fatalitiesByEvent,desc(FATALITIES))
  
  top5Fatalities <- fatalitiesByEvent[1:5,]
  
  return(top5Fatalities)

}
# ---- end top5FatalitiesFunc



# ---- top5InjuriesFunc
top5InjuriesFunc <- function(x) {

  injuriesByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(INJURIES = sum(INJURIES))

injuriesByEvent <- arrange(injuriesByEvent,desc(INJURIES))

top5injuries <- injuriesByEvent[1:5,]

return(top5injuries)

}
# ---- end top5InjuriesFunc



# ---- top5HealthFunc
top5HealthFunc <- function(x) {
  
  healthByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(TOTALHEALTH = sum(TOTALHEALTH))
  
  healthByEvent <- arrange(healthByEvent,desc(TOTALHEALTH))
  
  top5Health <- healthByEvent[1:5,]
  
  return(top5Health)
  
}
# ---- end top5InjuriesFunc




# ---- top5CropDmgFunc
top5CropDmgFunc <- function(x) {

  cropDmgByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(CROPDMG = sum(CROPDMG))
  
  cropDmgByEvent <- arrange(cropDmgByEvent, desc(CROPDMG))  
  
  top5CropDmg <- cropDmgByEvent[1:5,]

  return(top5CropDmg)
  
}
# ---- end top5CropDmgFunc


# ---- top5PropDmgFunc
top5PropDmgFunc <- function(x) {

  propDmgByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(PROPDMG = sum(PROPDMG))
  
  propDmgByEvent <- arrange(propDmgByEvent, desc(PROPDMG))  
  
  top5PropDmg <- propDmgByEvent[1:5,]
  
  return(top5PropDmg)

}
# ---- end top5PropDmgFunc



# ---- top5TotalDmgFunc
top5TotalDmgFunc <- function(x) {
  
  totalDmgByEvent <- x %>% 
    group_by(EVTYPE) %>%
    summarise(TOTALDMG = sum(TOTALDMG))
  
  totalDmgByEvent <- arrange(totalDmgByEvent, desc(TOTALDMG))  
  
  top5TotalDmg <- totalDmgByEvent[1:5,]
  
  return(top5TotalDmg)

}
# ---- end top5TotalDmgFunc
