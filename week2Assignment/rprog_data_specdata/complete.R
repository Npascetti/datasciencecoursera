complete <- function(directory, id=1:332) {
  ogDir <- getwd()
  fileList <- paste(sprintf("%03d", id), ".csv", sep="")
  setwd(directory)
  
  completeFrame <- data.frame("id" = id, "nobs" = rep(NA, length(id)))
  
  nobsVector <- NULL
  
  for (i in fileList) {
    x <- read.csv(i)
    nobsVector <- append(nobsVector, sum(complete.cases(x)))
  }
  
  completeFrame$nobs <- nobsVector 
  
  setwd(ogDir)
  
  completeFrame
}