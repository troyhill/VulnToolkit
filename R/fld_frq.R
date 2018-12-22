#' @title Flooding frequency
#'
#' @description Calculates the frequency of high tides flooding an elevation of interest.
#'
#' @details Calculates the frequency of high tides flooding an elevation of interest.
#' 
#' @usage fld.frq(z, ht, units = "percent")
#' 
#' @param z elevation of interest
#' @param ht a numeric vector of high tide levels
#' @param units units for output. Default is \code{percent}; the proportion of tides 
#' flooding elevation \code{x}. Alternatively, \code{tides} will return the number of 
#' flooding tides in the dataset
#' 
#' @return value the number or percent of high tides in \code{vec} that fall above \code{z}
#' 
#' @seealso \code{\link{fld.dur}}
#' 
#' @importFrom stats ecdf
#' 
#' @examples
#' # get a dataset of high/low tides
#' data(NL_6min_2013)
#' HT.NL <- HL(level = NL_6min_2013[, 2], time = NL_6min_2013[, 1], tides = "H")
#' a <- fld.frq(0.9117, HT.NL[, 1], units = "tides") # number of flooding tides at MHW
#' a / length(HT.NL[, 1]) # flooding tides as a percentage of all tides in time period
#' 
#' fld.frq(0.9117, HT.NL[,1], units = "percent") # check 
#' 
#' b <- as.numeric((NL_6min_2013[nrow(NL_6min_2013), 1] - 
#' NL_6min_2013[1, 1])) / 365.242 # fraction of year covered by dataset
#' 
#' elev.frq <- data.frame(elev = seq(from = 0, to = 1.5, by = 0.005))
#' 
#' # error thrown if units = "tides" and length(x) > length(ht)
#' fld.frq(elev.frq$elev, HT.NL[, 1], units = "tides") 
#' 
#' # a work-around
#' elev.frq$frq <- fld.frq(elev.frq$elev, HT.NL[, 1], units = "percent") * 
#' length(HT.NL[, 1]) / b 
#' 
#' plot(elev.frq$frq ~ elev.frq$elev, pch = 19, 
#' ylab = "flooding frequency (tides per year)", xlab = "elevation (m; MLLW)")
#' @export

fld.frq <- function(z, ht = z, units = "percent") {  
  if(is.numeric(z) == FALSE) stop("'z' must be a numeric value")
  if(is.numeric(ht) == FALSE) stop("high tide dataset ('ht') must be numeric")
  if(!units %in% c("tides", "percent"))  stop("'units' argument must be 'tides' or 'percent'")
  
  if(units == "tides"){
    length(ht[ht >= z])
  } else {
    a <- stats::ecdf(ht)
    1 - a(z)
  }

}
