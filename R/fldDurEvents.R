dur.events <- function(level, elevation, percentile = 0.5,
                       units = "1 observation") {

  # get durations of all flooding events at elevation x
  # find percentile y
  # units argument enables conversion from equi-temporal but undefined observations to actual time units
  #  must take the form "XX units" where 'XX' is an integer and 'units' can be second, minute, hour, or 
  #  the respective plural version. Units are converted to hours (or sequential observations, if no units are provided).
  # this must accept vectors for elevation and percentile arguments  
  # NAs must be dealt with before running this code - they are treated as exposed periods
  
  if (length(percentile) == 1) {
    durs <- as.numeric(NA)
    for (i in 1:length(elevation)) {
      
      # exclude dry periods
      ind <- ifelse(level >= elevation[i] & !is.na(level), 1, 0) 
      # calculates flooding event durations from run lengths (agnostic about time units)
      floodEvents <- rle(ind)$lengths[rle(ind)$values == 1]
      
      ifelse(percentile == 0.5, 
             # return depths relative to elevation of interest
             percentileDur <- median(floodEvents), # median() is faster than quantile
             percentileDur <- quantile(floodEvents, percentile)
      )
      durs[i] <- percentileDur
    }
  } else if (length(percentile) > 1) {
    durs <- data.frame(elev = elevation)
    for(i in 1:length(percentile)) {
      durs[, (i + 1)] <- as.numeric(NA)
    }
    for (i in 1:length(elevation)) {
      ind <- ifelse(level >= elevation[i] & !is.na(level), 1, 0) 
      floodEvents <- rle(ind)$lengths[rle(ind)$values == 1]
      
      percentileDur <- quantile(floodEvents, percentile)
      durs[i, 2:(length(percentile) + 1)] <- as.numeric(percentileDur)
      if (names(durs)[2] == "V2") {
        names(durs)[2:(length(percentile) + 1)] <- names(percentileDur)
      }
    }
  } else if (length(percentile) < 1) {
    stop("error: 'percentile' cannot have length zero")
  }
  
  if (!units == "1 observation") {
    num <- as.numeric(strsplit(units, " ")[[1]][1])
    timeUnits <- strsplit(units, " ")[[1]][2]
    if (timeUnits %in% c("second", "seconds")) {
      # conversion is complicated b/c output has two possible structures (and I don't want to convert the elevation column)
      ifelse(ncol(durs) == 1 || is.null(ncol(durs)), 
             durs <- durs * num / 60 / 60, # convert to hours
             durs[, 2:ncol(durs)] <- durs[, 2:ncol(durs)] * num / 60 / 60
             )
    } else if (timeUnits %in% c("minute", "minutes")) {
      ifelse(ncol(durs) == 1 || is.null(ncol(durs)), 
             durs <- durs * num / 60, # convert to hours
             durs[, 2:ncol(durs)] <- durs[, 2:ncol(durs)] * num / 60
      )
    } else if (timeUnits %in% c("hour", "hours")) {
      ifelse(ncol(durs) == 1 || is.null(ncol(durs)), 
             durs <- durs * num, # no conversion needed
             durs[, 2:ncol(durs)] <- durs[, 2:ncol(durs)] * num
      )
    } else stop("error: data units aren't recognized.")
  } 
  durs
}