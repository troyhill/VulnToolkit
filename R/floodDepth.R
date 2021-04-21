#' @title Calculates flooding depth above an elevation of interest
#' 
#' @description Flooding depths are calculated from water level or tide data. 
#' If water levels are used, the median (or other percentile) flooding depth 
#' is calculated based on all observations of flooded conditions (when water 
#' depth is equal to or greater than the elevation of interest). If a high/low 
#' tide dataset is used, flooding depth percentiles will be just for high tides 
#' that flood the selected elevation. The latter case includes only peak high 
#' water levels, and so will yield greater flooding depths for the same elevation.
#' 
#' Using data from a nearby, user-defined NOAA station, bias is estimated by comparing the 
#' relationship between flooding duration and elevation in datasets covering two time periods: 
#' (1) the time periodof water level logger deployment, and (2) the reference time period 
#' (some number of years).
#' 
#' @usage fld.depth(level, elevation, percentile = 0.5)
#' 
#' @param level a numeric vector of water levels or tide data
#' @param elevation elevation(s) of interest (marsh platform, 
#' MHW). A vector of elevations is accepted. Elevation should 
#' be in the same vertical datum as \code{level}
#' @param percentile the percentile(s) to calculate 
#' (median flooding depth is calculated by default). A 
#' vector of percentiles is accepted.
#' 
#' @return if 'percentile' argument is of length one, the value(s) 
#' returned are the corresponding flooding percentiles for the 
#' elevation(s) of interest. To ensure clarity when more than one 
#' 'percentiles' are sought, in those cases a dataframe is output 
#' to report elevations and flooding depths.
#' 
#' @seealso \code{\link{noaa}}, \code{\link{fld.dur}}
#' 
#' 
#' @importFrom stats median
#' @importFrom stats quantile
#' 
#' @examples 
#' data(NL_6min_2013)
#' MHW <- 0.9117  # New London MHW in 2013: 0.9117 m relative to MLLW
#' fld.depth(level = NL_6min_2013[, 2], elevation = MHW,
#'          percentile = 0.5)
#'
#' fld.depth(level = NL_6min_2013[, 2], elevation = MHW, 
#'          percentile = c(0.25, 0.5, 0.75))
#'
#' fld.depth(level = NL_6min_2013[, 2], elevation = c(0, MHW, 1.5), 
#'          percentile = 0.75)
#'
#' fld.depth(level = NL_6min_2013[, 2], elevation = c(0, MHW, 1.5), 
#'          percentile = c(0.25, 0.5, 0.75))
#' @export




fld.depth <- function(level, elevation, percentile = 0.5) {
  if (length(percentile) == 1) {
    A <- as.numeric(NA)
    for (i in 1:length(elevation)) {
      # exclude dry periods 
      flooded <- level[level >= elevation[i] & !is.na(level)]
      ifelse(percentile == 0.5, 
         # return depths relative to elevation of interest
         percentileDepth <- stats::median(flooded) - elevation[i], # median() is faster than quantile
         percentileDepth <- stats::quantile(flooded, percentile) - elevation[i]
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