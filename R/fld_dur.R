#' @title Flooding duration
#'
#' @description
#' Calculates the percent of time an elevation is submerged
#'
#' @details
#' @usage /code{fld.frq(z, levels)}
#' @param z elevation of interest
#' levels a numeric vector of water levels. Time interval between measurements must be uniform.
#' @return value the decimal fraction of measurements in /code{vec} that fall above /code{z}
#' @seealso 
#' @references none
#' @aliases 
#' @keywords 
#' @export
#' @examples
#' data(NL_6min_2013)
#' a <- fld.dur(0.9117, NL_6min_2013[,2]); a # flooding duration at MHW
#' a * length(NL_6min_2013[,2]) / 10 # convert to hours per year
#' 
#' b <- fld.dur(0, NL_6min_2013[,2]); b # flooding duration at MLLW
#' b * length(NL_6min_2013[,2]) / 10 # hours per year
#' 
#' elev.dur <- data.frame(elev = seq(from = -0.5, to = 1.25, by = 0.005))
#' elev.dur$dur <- fld.dur(elev.dur$elev, NL_6min_2013[,2]) * length(NL_6min_2013[,2]) / 10 
#' 
#' plot(elev.dur$dur ~ elev.dur$elev, pch = 19, ylab = "flooding duration (hours per year)", xlab = "elevation (m; MLLW)")


fld.dur <- function(z, levels = z) {  
  if(is.numeric(z) == FALSE) stop("'z' must be a numeric value")
  if(is.numeric(levels) == FALSE) stop("water level dataset ('levels') must be numeric")

  a <- ecdf(levels)
  1 - a(z)
}
