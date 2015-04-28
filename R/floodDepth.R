fld.depth <- function(level, elevation, percentile = 0.5) {
  if (length(percentile) == 1) {
    A <- as.numeric(NA)
    for (i in 1:length(elevation)) {
      # exclude dry periods 
      flooded <- level[level >= elevation[i] & !is.na(level)]
      ifelse(percentile == 0.5, 
         # return depths relative to elevation of interest
         percentileDepth <- median(flooded, percentile)   - elevation[i], # median() is faster than quantile
         percentileDepth <- quantile(flooded, percentile) - elevation[i]
      )
    A[i] <- percentileDepth
  }
  } else if (length(percentile) > 1) {
    A <- data.frame(elev = elevation)
    for(i in 1:length(percentile)) {
      A[, (i + 1)] <- as.numeric(NA)
    }
    for (i in 1:length(elevation)) {
      flooded <- level[level >= elevation[i] & !is.na(level)]
      percentileDepth <- quantile(flooded, percentile) - elevation[i]
      A[i, 2:(length(percentile) + 1)] <- as.numeric(percentileDepth)
      if (names(A)[2] == "V2") {
        names(A)[2:(length(percentile) + 1)] <- names(percentileDepth)
      }
    }
  } else if (length(percentile) < 1) {
    stop("error: 'percentile' cannot have length zero")
  }
  A
}