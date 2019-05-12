pollutantmean <- function(directory, pollutant, id = 1:332) {
  ogDir <- getwd()
  fileList <- paste(sprintf("%03d", id), ".csv", sep="")
  setwd(directory)
  rowCount <- 0
  sumCount <- 0
  for (i in fileList) {
    x <- read.csv(i)
    y <- x[pollutant]
    z <- y[complete.cases(y),]

    rowCount <- rowCount + length(z)
    sumCount <- sumCount + sum(z)

  }
  setwd(ogDir)
  (sumCount/rowCount)
}