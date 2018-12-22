#' @title Plots water level data and high/low tides extracted by \code{HL()}
#' 
#' @description  Plots water level data and high/low tides extracted 
#' by \code{HL()}. Purpose is for quick and easy visual assessment 
#' of \code{HL()} output.
#' 
#' @usage HL.plot(level, time, period = 13, phantom = TRUE, tides = "all", semidiurnal = TRUE)
#' 
#' @param level a numeric vector of water levels
#' @param time a vector (numeric or POSIX*) indicating the time of water 
#' level measurements. Units must be minutes.
#' @param period a single numeric or integer estimate of tidal period 
#' (full tidal cycle). Units must be hours.
#' @param phantom a protective measure taken to prevent the inclusion of an 
#' artificial high or low tide at the end of the dataset. If the water level 
#' measurements end precisely at a low or high tide, this can be changed to FALSE.
#' @param tides is used to optionally subset the output to include 
#' only high or low tides. This argument can be 'all' (default), 'H', or 'L'
#' @param semidiurnal logical. If TRUE, higher- and lower- high/low tides are reported in a separate column called 'tide2'
#' 
#' 
#' @return a plot of water levels, with red and blue dots superimposed on high and low tides.
#' 
#' @seealso \code{\link{HL}}
#' 
#' @references \url{http://wetlandsandr.wordpress.com/}
#' 
#' 
#' @importFrom graphics plot
#' @importFrom graphics points
#' 
#' @examples 
#' data(NL_6min_2013)
#' HL.plot(level = NL_6min_2013[,2], time = NL_6min_2013[,1])
#' HL.plot(level = NL_6min_2013[1:1000,2], time = NL_6min_2013[1:1000,1]) 
#' @export


HL.plot <- function(level, time, period = 13, phantom = TRUE, tides = "all", semidiurnal = TRUE) {
  hl <- VulnToolkit::HL(level = level, time = time, period = period, phantom = phantom, tides = tides, semidiurnal = semidiurnal)
  wll.2 <- data.frame(1:length(level), level, time)
  
  graphics::plot(wll.2$level ~ wll.2$time, type = "l", ylab = "water level", 
       xlab = "", xaxt = "n", col = "darkgray")
  graphics::points(hl$level[hl$tide == "H"]  ~ hl$time[hl$tide == "H"], pch = 19, cex = 0.75, col="red")
  graphics::points(hl$level[hl$tide == "L"] ~ hl$time[hl$tide == "L"], pch = 19, cex = 0.75, col="cornflowerblue")
}
