wave.dur <- function(level, elevation, buffer = 0.050){
  # observations within buffer
  if (length(elevation) == 1) {
  zone <- length(level[level >= (elevation - buffer) & level <= (elevation + buffer)  & !is.na(level)])
  denom <- length(na.omit(level))
  } else if (length(elevation) > 1){
    zone <- as.numeric(NA)
    denom <- length(na.omit(level))
    
    for (i in 1:length(elevation)){
    zone[i] <- length(level[level >= (elevation[i] - buffer) & level <= (elevation[i] + buffer)  & !is.na(level)])
    }
  } else if (length(elevation) < 1){
    stop ("error: `elevation` argument must be of length >= 1")
  }
  
  zone / denom
  
}

