#' @title Find the proportion of time when water surface is near a selected elevation
#' 
#' @description Find the proportion of time when water surface is near a selected elevation
#' 
#' @usage wave.dur(level, elevation, buffer = 0.050)
#' 
#' @param level a numeric vector of water levels
#' @param elevation elevation(s) of interest (e.g., marsh platform, MHW). A vector of 
#' elevations is accepted. Elevation should be in the same vertical datum 
#' as \code{level} and have units of meters
#' @param buffer the vertical range used to calculate exposure. Default 
#' is 0.050 meters (5 cm)
#' 
#' @return a vector of values representing the fraction of time in the entire dataset 
#' where the water surface is near the elevation of interest. Numerical range is from 0-1
#' 
#' @importFrom stats na.omit
#'  
#' @examples data(NL_6min_2013)
#' MHW <- 0.9117  # New London MHW in 2013: 0.9117 m relative to MLLW
#' 
#' wave.dur(NL_6min_2013[, 2], MHW)
#' 
#' # function accepts and returns vectors
#' # elevs           <- data.frame(elevs = c(-1000:1500) / 10^3)
#' # elevs$wave.dur  <- wave.dur(NL_6min_2013[, 2], elevs$elevs)
#' # plot(elevs)
#' @export


wave.dur <- function(level, elevation, buffer = 0.050){
  # observations within buffer
  if (length(elevation) == 1) {
  zone <- length(level[level >= (elevation - buffer) & level <= (elevation + buffer)  & !is.na(level)])
  denom <- length(na.omit(level))
  } else if (length(elevation) > 1){
    zone <- as.numeric(NA)
    denom <- length(stats::na.omit(level))
    
    for (i in 1:length(elevation)){
    zone[i] <- length(level[level >= (elevation[i] - buffer) & level <= (elevation[i] + buffer)  & !is.na(level)])
    }
  } else if (length(elevation) < 1){
    stop ("error: `elevation` argument must be of length >= 1")
  }
  
  zone / denom
  
}

