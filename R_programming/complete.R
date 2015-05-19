
complete <- function(directory, id = 1:332) {


  if (file.exists(directory)) {

    totalDF <- data.frame(id=numeric(0), nobs=numeric(0))
    for (i in id) {

      fileName <- sprintf("%s/%03d.csv",directory,i)
      
      if (file.exists(fileName)) {
        
          data <- read.csv(fileName)
          df <- NULL
          df <- rbind (df, data)
          good <- complete.cases(df)
          goodData <- df[good,][,]
          numRows <- nrow(goodData)
          if (numRows > 0) {
            totalDF[nrow(totalDF)+1, ] <- c(i, numRows)
          }
      } else {
        next
      }
      
    }

    totalDF
    
  } else {
    stop(cat("directory " , directory , " does not exist"))
  }
  
}