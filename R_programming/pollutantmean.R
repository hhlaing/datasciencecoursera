

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ##'directory' is character vector of length 1 indicating the location of the csv files

  ## 'polutant' is character vector of length 1 for name of polluant; either sulfate or nitrate
  
  ## 'id is an integer vector indicating the monitor ID numbers to be used
  
  ## Return the mean of the polluant across all monitors list in the 'id' vector (ignoring NA's)
  
  if (file.exists(directory)) {
    message('directory = ', directory)
    setwd(directory)
    fileList = c()

    for (i in id) {
      fileName <- sprintf("%03d.csv",i)
      if (file.exists(fileName)) {
        fileList <- c(fileList, fileName)
      } else {
        warning("file " , fileName, " does not exist")
      }
    }

    df <- NULL
    for (f in fileList) {
      data <- read.csv(f)
      df <-rbind(df, data)
    }

    pollutantVector <- df[pollutant]
    bad <- is.na(pollutantVector)
    completePollutantVector <- pollutantVector[!bad]
    mean(completePollutantVector)
  } else {
    stop(cat("directory " , directory , " does not exist"))
  }
  
  
  
  
}