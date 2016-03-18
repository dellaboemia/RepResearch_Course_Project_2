# Coursera Data Science Specialization
# Reproducible Research
# Course Project 2
# Author: John James

# Date: March 7, 2016
# review.R

## ---- reviewDataFunc
reviewData <- function(x) {
  # This function performs a series of diagnostic reports to ascertain the structure, validity, and characteristics of
  # the data 
  #
  # Args:
  #   x   - the data frame to be reviewed
  #
  # Response: This function will load the file into a data frame and return it to the calling script
  
  # Validate arguments
  if (missing(x)) { stop("The data frame to be reviewed must be provided") }
  
  
  # Check dimensions of the data set
  d <- dim(x)
  
  # Get unique event types
  e <- unique(x$EVTYPE)
  e <- length(e)

  # Get number of observations with all zero values for fatalities, injuries, and damage.
  z <- x[ which(x$FATALITIES == 0 & x$INJURIES == 0 & x$PROPDMG == 0 & x$CROPDMG == 0),]
  z <- dim(z)
  

  # Create list and return it.
  l <- list(d, e, z)
  return(l)

}
## ---- end reviewDataFunc
