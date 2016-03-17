# Public Health and Economic Impact of Hydro-Meteorological Events (Storms)
John James  
March 6, 2016  



## Synopsis

## Introduction
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

The purpose of this project was to explore and analyze the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database to determine which types of hydro-meteorological events posed the greatest public health and economic cost.

## Research Question
The report seeks to address the  following question:

1. Across the United States, which types of events are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?




## Data 
This report analyzed the NOAA Storm Events Database which documents the occurrence of storms and other significant weather phenomena having sufficient intensity to cause loss of life, injuries, significant property damage, and/or disruption to commerce. The database currently contains data from January 1950 to November 2015, as entered by NOAA's National Weather Service (NWS), capturing fatalities, property damage, and crop damage for some 48 designated event types.



## Data Processing
### Environment Setup
The following code configures the environment for the analysis.  Filenames are designated, the directory structure is established, requisite files and libraries are sourced. 



### Load Data

The data consisted of 902,297 observations of 37 variables.  The variables of interest for this analysis were:

* EVTYPE - The type of event.  The data contained 985 factor levels.
* FATALITIES - The number of fatalities directly caused by the weather event
* INJURIES - The number of persons injured as a direct consequence of the weather event.
* PROPDMG - The estimated amount of property damage indicated in dollar amounts adjusted by the PROPDMGEXP factor
* PROPDMGEXP - This factor indicates multiplicative factor to apply to the PROPDMG number.  For instance, a value of "K" indicates that the number in PROPDMG should be multiplied by 1000.  
* CROPDMG - The estimated amount of crop damage indicated in dollar amounts adjusted by the PROPDMGEXP factor
* CROPDMGEXP - This factor indicates multiplicative factor to apply to the CROPDMG number.  For instance, a value of "2" indicates that the number in PROPDMG should be multiplied by 100.  

### Data Transformations
Before analyzing the data, several transformations were performed.

#### Event Type
There were 48 designated event types; however, there were 985 different factor levels for EVTYPE.  A script was written to scan each event type, compare it to the list of the designated event types, then create a new factor variable "EVENTCAT", that contained the correctly encoded value. The designated event types data frame was manually created from the NOAA Storm Data Directive NWSPD 10-16 which is available on the NOAA website at https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf. 


## Data Analysis

## Results

## References
(NOAA), U. N. (2016, March 8). NOAA Storm Database. Retrieved from NOAA National Centers for Environmental Information: https://www.ncdc.noaa.gov/stormevents/


### Load Data





