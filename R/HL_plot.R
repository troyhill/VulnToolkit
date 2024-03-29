#' @title Plots water level data and high/low tides extracted by \code{HL()}
#' 
#' @description  Plots water level data and high/low tides extracted 
#' by \code{HL()}. Purpose is for quick and easy visual assessment 
#' of \code{HL()} output.
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
#' @param verbose logical. If TRUE, messages are returned to console identifying the presence of NAs in the input data
#' 
#' @return a plot of water levels, with red and blue dots superimposed on high and low tides.
#' 
#' @seealso \code{\link{HL}}
#' 
#' 
#' 
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom graphics axis
#' 
#' @examples 
#' HL.plot(level = NL_6min_2013[,2], time = NL_6min_2013[,1])
#' HL.plot(level = NL_6min_2013[1:1000,2], time = NL_6min_2013[1:1000,1]) 
#' 
#' ### HL.plot can be annotated using base graphics
#' abline(h = -0.3, lty = 2) 
#' text(x = as.POSIXct("2013-01-01 00:00", format = "%F %R"), y = -0.25, 
#'      "elevation of interest", pos = 4)
#' mtext("plot title", side = 3)
#' 
#' ### or extended with ggplot in various ways. 
#' \dontrun{
#' library(ggplot2) # install with install.packages("ggplot2")
#' ggHL <- function(level, time, plotVariable = "tide2", period = 13, 
#'         phantom = TRUE, tides = "all", semidiurnal = TRUE) {
#'   hl <- VulnToolkit::HL(level = level, time = time, period = period,
#'                         phantom = phantom, tides = tides, semidiurnal = semidiurnal)
#'   wll.2 <- data.frame(1:length(level), level, time)
#'   plot_return <- ggplot(data = wll.2, aes(x = time, y = level)) + 
#'     geom_line(col = "darkgray") + xlab("") +
#'     geom_point(data = hl, aes(x = time, y = level, col = get(plotVariable))) +
#'     theme_classic() + theme(legend.title = element_blank())
#'   plot_return
#' }
#' newPlot <- ggHL(level = NL_6min_2013[1:1000,2], time = NL_6min_2013[1:1000,1])
#' newPlot
#' }
#' 
#' 
#' @export


HL.plot <- function(level, time, period = 13, phantom = TRUE, tides = "all", semidiurnal = TRUE, verbose = FALSE) { # nocov start
  hl <- VulnToolkit::HL(level = level, time = time, period = period, phantom = phantom, tides = tides, semidiurnal = semidiurnal, verbose = verbose)
  wll.2 <- data.frame(1:length(level), level, time)
  
  graphics::plot(wll.2$level[!is.na(wll.2$level)] ~ wll.2$time[!is.na(wll.2$level)], type = "l",# type = "p", pch = 19, cex = 0.5, 
                 ylab = "water level", 
       xlab = "", xaxt = "n", col = "darkgray")
  graphics::points(hl$level[hl$tide == "H"]  ~ hl$time[hl$tide == "H"], pch = 19, cex = 0.75, col="red")
  graphics::points(hl$level[hl$tide == "L"] ~ hl$time[hl$tide == "L"], pch = 19, cex = 0.75, col="cornflowerblue")
  ### include high tide times on x axis. not accommodating to long time series...
  if (grepl(x = tides, pattern = "all|H")) {
    tick_times <- hl$time[hl$tide == "H"]
  } else {
    tick_times <- hl$time[hl$tide == "L"]
  }
  if (is.numeric(time)) {
    graphics::axis(side = 1, at=tick_times, labels=tick_times)
  } else if(inherits(time, "POSIXt") | inherits(time, "Date")) { # assume it's date/posix otherwise
    graphics::axis(side = 1, at=tick_times, labels=format(tick_times,"%d %b %H:%M"))
  }
} 
