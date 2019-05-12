corr <- function(directory, threshold = 0) {
  ogDir <- getwd()
  setwd(directory)
  
  fileList <- list.files()
  
  nobsVector <- NULL
  idVector <- NULL
  
  completeFrameThreshold <- data.frame("id" = rep(NA, length(fileList)), "nobs" = rep(NA, length(fileList)))
  
  for (file in fileList) {
    x <- read.csv(file)
    if (sum(complete.cases(x)) >= threshold) {
    nobsVector <- append(nobsVector, sum(complete.cases(x)))
    idVector <- append(idVector, x$ID[1])
    }
    else {
    nobsVector <- append(nobsVector, NA)
    idVector <- append(idVector, x$ID[1])
    }
  }
  completeFrameThreshold$nobs <- nobsVector
  completeFrameThreshold$id <- idVector
  
  ###make vector of ids we want to perform corellation on
  vectorIdToCor <- completeFrameThreshold$id[complete.cases(completeFrameThreshold)]
  
  ###if the valid id vector is empty, return empty numeric vector
  if (sum(vectorIdToCor) == 0) {
    setwd(ogDir)
    return(vectorIdToCor)
  }
  
  corFileList <- paste(sprintf("%03d", vectorIdToCor), ".csv", sep="")
  finalCorVector <- numeric()
  
  for (file in corFileList) {
    x <- read.csv(file)
    y <- cor(x$sulfate, x$nitrate, use = "pairwise.complete.obs")
    finalCorVector <- append(finalCorVector, y)
  }
  
  finalCorVector <- na.omit(finalCorVector)
  
  setwd(ogDir)
  
  finalCorVector
}