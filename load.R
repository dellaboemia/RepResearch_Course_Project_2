# Coursera Data Science Specialization
# Reproducible Research
# Course Project 2
# Author: John James
# Date: March 5, 2016
# load.R

## ---- loadDataFunc
loadData <- function(url, dir, zip, csv, df ) {
  # This function downloads a zipped file from the web and stores it in the requested library. Note: This script requires that the current working directory be
  # the top directory for the project.
  # Args:
  #   url - the url for website from which the file will be downloaded
  #   dir - the redirectory to which the file will be stored
  #   zip - the name of the zip file
  #   csv - the name of the csv file
  #   df  - the name of thd data frame that contains the data
  #
  # Response: This function will load the file into a data frame and return it to the calling script
  
  # Validate arguments
  if (missing(url)) { stop("Missing url for data") }
  if (missing(dir)) { stop("Directory to which the data is to be stored, must be specified") }
  if (missing(zip)) { stop("The name of the zip file must be specified") }
  if (missing(csv)) { stop("The name of the csv file must be specified") }
  if (missing(df))  { stop("The name of the data frame must be specified") }
  
  # Set filenames
  zip <- paste(dir,zip, sep="")
  csv <- paste(dir,csv, sep="")
  
  
  # Create data directory if does not already exist
  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  # Download the raw data and read the csv file if the zip file doesn't exist or it wasn't downloaded today.
  today <- Sys.Date()
  cDate <- as.Date(file.info(zip)$mtime, tz="EST")
  if ((!file.exists(zip)) | (cDate != today)){
    download(url, dest = zip, mode="wb")
    df <- read.csv(bzfile(zip))
  }

  # Read csv file into data frame
  df <- read.csv(bzfile(zip))
  
  return(df)
}
# ---- end
