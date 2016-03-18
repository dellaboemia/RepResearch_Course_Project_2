# Coursera Data Science Specialization
# Reproducible Research
# Course Project 2
# Author: John James
# Date: March 7, 2016
# process.R


#############################################################################
##                          PROCESS FUNCTIONS                              ##
#############################################################################

## ---- processEventTypeFunc

processEventType <- function(x) {
  # This function reads the EVTYPE variable from the storm data frame, then replaces it with one of the correctly encoded
  # and designated event types from the eventType data frame.
  #
  # Args:
  #   x   - the NOAA storm data frame
  #
  # Response: Returns the storm data frame with the new factor variable containing the designated event type
  
  # Change event types to lower case
  x$EVTYPE = tolower(x$EVTYPE)
  
  # Clean up event types
  x$EVTYPE[grep("low tide", x$EVTYPE)] = "Astronomical Low Tide"
  x$EVTYPE[grep("avalanche|avalanc", x$EVTYPE)] = "Avalanche"
  x$EVTYPE[grep("blizzard|snow|excessive snow", x$EVTYPE)] = "Snow"
  x$EVTYPE[grep("coastal flood|beach|erosion", x$EVTYPE)] = "Coastal Flood"
  x$EVTYPE[grep("wind chill", x$EVTYPE)] = "Cold Wind Chill"
  x$EVTYPE[grep("debris flow|rock|landslide|mud|lands", x$EVTYPE)] = "Debris Flow"
  x$EVTYPE[grep("dense fog|vog|fog", x$EVTYPE)] = "Dense Fog"
  x$EVTYPE[grep("smoke", x$EVTYPE)] = "Dense Smoke"
  x$EVTYPE[grep("drought|dry|driest", x$EVTYPE)] = "Drought"
  x$EVTYPE[grep("dust", x$EVTYPE)] = "Dust Devil/Storm"
  x$EVTYPE[grep("heat|high temp|warmth|hot|high|warm|record high|record temp", x$EVTYPE)] = "Heat"
  x$EVTYPE[grep("lakeshore flood", x$EVTYPE)] = "Lakeshore Flood"
  x$EVTYPE[grep("flash flood|dam|fld|stream|rapidly rising", x$EVTYPE)] = "Flash Flood"
  x$EVTYPE[grep("flood|wet", x$EVTYPE)] = "Flood"
  x$EVTYPE[grep("freeze|cold|frost|low temp|freezing|hyperthermia|cool|ice|icy", x$EVTYPE)] = "Frost/Freeze"
  x$EVTYPE[grep("tornado|funnel|torndao|gustnado", x$EVTYPE)] = "Tornados"
  x$EVTYPE[grep("freezing fog", x$EVTYPE)] = "Freezing Fog"
  x$EVTYPE[grep("hail", x$EVTYPE)] = "Hail"
  x$EVTYPE[grep("heavy rain|hvy rain|rain|precip|wet|shower", x$EVTYPE)] = "Heavy Rain"
  x$EVTYPE[grep("heavy snow", x$EVTYPE)] = "Heavy Snow"
  x$EVTYPE[grep("surf|seas|wave", x$EVTYPE)] = "High Surf"
  x$EVTYPE[grep("high wind|wnd|turbulence|burst", x$EVTYPE)] = "High Wind"
  x$EVTYPE[grep("extreme cold|low", x$EVTYPE)] = "Extreme Cold/Wind Chill"
  x$EVTYPE[grep("typhoon|hurricane", x$EVTYPE)] = "Hurricane (Typhoon)"
  x$EVTYPE[grep("ice", x$EVTYPE)] = "Ice Storm"
  x$EVTYPE[grep("lake|effect", x$EVTYPE)] = "Lake-Effect Snow"
  x$EVTYPE[grep("lightning|lighting|ligntn", x$EVTYPE)] = "Lightning"
  x$EVTYPE[grep("marine hail", x$EVTYPE)] = "Marine Hail"
  x$EVTYPE[grep("marine high wind", x$EVTYPE)] = "Marine High Wind"
  x$EVTYPE[grep("marine strong wind", x$EVTYPE)] = "Marine Strong Wind"
  x$EVTYPE[grep("marine thunderstorm wind|marine wind", x$EVTYPE)] = "Marine Thunderstorm Wind"
  x$EVTYPE[grep("marine mishap|marine accident|drowning", x$EVTYPE)] = "Marine Accident"
  x$EVTYPE[grep("current", x$EVTYPE)] = "Rip Current"
  x$EVTYPE[grep("seiche", x$EVTYPE)] = "Seiche"
  x$EVTYPE[grep("sleet", x$EVTYPE)] = "Sleet"
  x$EVTYPE[grep("surge|tide|swells", x$EVTYPE)] = "Storm Surge/Tide"
  x$EVTYPE[grep("strong wind", x$EVTYPE)] = "Strong Wind"
  x$EVTYPE[grep("thunderstorm|wind|tstm", x$EVTYPE)] = "Thunderstorm Wind"
  x$EVTYPE[grep("tropical depression", x$EVTYPE)] = "Tropical Depression"
  x$EVTYPE[grep("tropical storm", x$EVTYPE)] = "Tropical Storm"
  x$EVTYPE[grep("tsunami", x$EVTYPE)] = "Tsunami"
  x$EVTYPE[grep("volcanic|volcano", x$EVTYPE)] = "Volcanic Ash"
  x$EVTYPE[grep("spout", x$EVTYPE)] = "Waterspout"
  x$EVTYPE[grep("wildfire|fire", x$EVTYPE)] = "Wildfire"
  x$EVTYPE[grep("winter weather|wintry|mix", x$EVTYPE)] = "Winter Weather"
  x$EVTYPE[grep("storm", x$EVTYPE)] = "Winter Storms"
  x$EVTYPE[grep("summary|glaze|//?|other|urban", x$EVTYPE)] = "Other"
  
  return(x)
  
}
## ---- end

## ---- processDamageFunc
processDamage <- function(x) {
  # This function processes the damage exponents and adjusts the crop and property damage estimates accordingly 
  #
  # Args:
  #   x   - the NOAA storm data frame
  #
  # Response: Returns the storm data frame with damage estimates in dollar terms.
  
  # Set exponents
  x$PROPDMGEXP = tolower(x$PROPDMGEXP)
  x$PROPDMGEXP[grep("", x$PROPDMGEXP)] = "0"
  x$PROPDMGEXP[grep("+", x$PROPDMGEXP)] = "0"
  x$PROPDMGEXP[grep("?", x$PROPDMGEXP)] = "1"
  x$PROPDMGEXP[grep("h", x$PROPDMGEXP)] = "2"
  x$PROPDMGEXP[grep("k", x$PROPDMGEXP)] = "3"
  x$PROPDMGEXP[grep("m", x$PROPDMGEXP)] = "6"
  x$PROPDMGEXP[grep("b", x$PROPDMGEXP)] = "9"
  
  x$CROPDMGEXP = tolower(x$CROPDMGEXP)
  x$CROPDMGEXP[grep("", x$CROPDMGEXP)] = "0"
  x$CROPDMGEXP[grep("+", x$CROPDMGEXP)] = "0"
  x$CROPDMGEXP[grep("?", x$CROPDMGEXP)] = "1"
  x$CROPDMGEXP[grep("h", x$CROPDMGEXP)] = "2"
  x$CROPDMGEXP[grep("k", x$CROPDMGEXP)] = "3"
  x$CROPDMGEXP[grep("m", x$CROPDMGEXP)] = "6"
  x$CROPDMGEXP[grep("b", x$CROPDMGEXP)] = "9"
  
  x$PROPDMG = as.numeric(x$PROPDMG) * 10 ^ as.numeric(x$PROPDMGEXP)

  x$CROPDMG = as.numeric(x$CROPDMG) * 10 ^ as.numeric(x$CROPDMGEXP)
  
  x$TOTALDMG <- x$PROPDMG + x$CROPDMG
  
  x$TOTALHEALTH <- x$FATALITIES + x$INJURIES

  return(x)  
} 
## ---- end

## ---- processYearFunc
processYear <- function(x) {
  # This function adds the year of the event 
  #
  # Args:
  #   x   - the NOAA storm data frame
  #
  # Response: Returns the storm data frame with the year of the event aded as a variable
  x$BGN_DATE <- as.Date(x$BGN_DATE, "%m/%d/%Y")
  x$YEAR <- format(x$BGN_DATE, "%Y")
  
  return(x)
  
}
# ---- end