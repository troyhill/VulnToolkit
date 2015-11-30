#' @title Calculates the duration of the median (or other percentile) flooding event
#'
#' @description Calculates the duration of the median (or other percentile) flooding event
#'
#' @details The duration of individual flooding events is calculated from water level data.
#'  \code{dur.events()} differs from \code{fld.dur()} in that the former examines 
#'  individual flooding events, rather than cumulative inundation times.
#' 
#' @usage dur.events(level, elevation, percentile = 0.5, units = "1 observation")
#' 
#' @param level a numeric vector of water levels or tide data
#' 
#' @param elevation elevation(s) of interest (e.g., marsh platform, MHW). A vector 
#' of elevations is accepted. Elevation should be in the same vertical datum as \code{level}
#' 
#' @param percentile the percentile(s) to find (median flooding event is used by default)
#' 
#' @param units the time interval between measurements. This argument enables conversion of 
#' flooding durations to hours. \code{units} must be a string of the form \code{XX units}, 
#' where \code{XX} is an integer and \code{units} can be \code{second}, \code{minute}, 
#' \code{hour}, or their plurals. When time units are specified, all time units are 
#' converted to hours in output. When units are not specified, the output reports the 
#' number of sequential observations
#' 
#' @return the duration of the median (or other percentile) flooding event occurring at a given 
#' elevation. If \code{percentile} is of length one, the value(s) returned are the 
#' corresponding flooding duration percentiles for the elevation(s) of interest. To ensure 
#' clarity when more than one \code{percentile} is sought, in those cases a dataframe is 
#' output to report elevations and flooding durations
#' 
#' @seealso \code{\link{noaa.stations}}
#' 
#' @examples
#' data(NL_6min_2013)
#' MHW <- 0.9117  # New London MHW in 2013: 0.9117 m relative to MLLW
#' 
#' # median flooding duration at three elevations
#' dur.events(NL_6min_2013[, 2], elevation = c(0.5, MHW, 1.5), units = "6 minutes")
#' 
#' # a dataframe is output when results are more complex
#' dur.events(NL_6min_2013[, 2], elevation = MHW, 
#'            percentile = c(0.1, 0.5, 0.9), units = "6 minutes")
#' dur.events(NL_6min_2013[, 2], elevation = c(0.5, MHW, 1.5), 
#'   percentile = c(0.1, 0.5, 0.9), units = "6 minutes")
#' 
#' # dur.events() differs from fld.dur() in that it examines individual
#' # flooding events, rather than cumulative inundation
#' 
#' # The median flooding event at MHW lasts 2.9 hrs
#' dur.events(NL_6min_2013[, 2], elevation = MHW, units = "6 minutes")
#' 
#' # And over an entire year, MHW was flooded 13% of the time
#' fld.dur(z = MHW, NL_6min_2013[, 2])
#' @export 


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