complete <- function(directory, id = 1:332) {
    
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
        warning("file " , fileName, " does not exist")
      }
    }
    
    totalDF
  
}


corr <- function(directory, threshold = 0) {
  
  if (file.exists(directory)) {
    #setwd(directory)

    files <- list.files(directory)
    result <- c()
    compDataFrame <- complete( directory, 1:length(files))

    dataForCor <- subset(compDataFrame, nobs > threshold )
    resultVector <- c()

    if (nrow(dataForCor) > 0) {
      for (i in 1:nrow(dataForCor)) {
        fid <- dataForCor[i,1]
        
        fileName <- sprintf("%s/%03d.csv",directory,fid[1])
        data <- read.csv(fileName)
        df <- NULL
        df <- rbind(df, data)
        sulfate <- df["sulfate"]
        nitrate <- df["nitrate"]
        
        
        result <- cor(sulfate,nitrate, use="pairwise.complete.obs")
        
        resultVector <- append(resultVector, result)
        
        
      }
    }
    
    resultVector

  } else {
    stop(cat("directory " , directory , " does not exist"))
  }
  
}