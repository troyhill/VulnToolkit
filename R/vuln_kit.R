#' @title Calculates selected hydrologic parameters and vulnerability metrics
#' 
#' @description This function takes a set of water level data as an input, and 
#' calculates a set of flooding parameters.
#' 
#' @usage vuln.kit(level, datetime, platform, units = "meters", frq.dur.inc = 0.005, 
#' TV.inc = 0.1, period = 13, filename = "VTK_output.png")
#' 
#' @param level a numeric vector of water levels
#' @param datetime a POSIX* vector of time stamps that correspond to the measurements 
#' in \code{level}
#' @param platform elevation of the marsh platform (or another vertical position of 
#' interest). Should be in the units specified by \code{units} and relative to the 
#' same vertical datum as \code{level}
#' @param units length units used. 'meters' is default; 'feet' is alternative. If 
#' units are 'feet', data is converted internally and output in meters
#' @param frq.dur.inc elevation interval used to calculate flooding frequency, 
#' duration, D90, and Ax. Defaults to 0.005 m. Units must correspond to \code{units} argument
#' @param TV.inc elevation interval used to calculate vulnerability metrics 
#' (DV, D90V). Defaults to 0.1 m. Units must correspond to \code{units} argument
#' @param period estimated time between consecutive high tides. Default ('13') is set for 
#' semi-diurnal systems
#' @param filename name of file showing output graphics. File is saved to the working 
#' directory.
#' 
#' @return a figure is returned, showing how flooding parameters vary as a function of 
#' elevation. Output also includes a list that contains two items:
#' a dataframe of elevations (relative to elevation set in \code{platform} argument), 
#' flooding frequencies (percent of tides flooding each elevation), flooding durations 
#' (percent of time flooded), duration of 90th percentile flooding event (D90; hr), and 
#' mean flooding depth (A; m); and
#' a dataframe containing the flooding frequency, flooding duration, D90, mean flooding 
#' depth, duration vulnerability, and D90 vulnerability, calculated at the vertical 
#' elevation set by \code{platform} argument
#' 
#' @importFrom stats quantile
#' @importFrom grDevices png
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics axis
#' @importFrom graphics mtext
#' @importFrom grDevices dev.off
#' 
#' @examples # data(NL_6min_2013)
#' # NL2013 <- vuln.kit(level = NL_6min_2013[,2], datetime = NL_6min_2013[,1],
#' #                   platform = 0.9117) # MHW in 2013: 0.9117 m relative to MLLW
#' # NL2013$metrics
#' @export
 

vuln.kit <- function(level, datetime, platform, units = "meters", frq.dur.inc = 0.005, 
                    TV.inc = 0.1, period = 13, filename = "VTK_output.png") {
 
  if (is.numeric(level) == FALSE)
    stop("invalid entry: 'level' must be numeric")
  
  if (class(datetime)[1] != "POSIXlt" & class(datetime)[1] != "POSIXct" )
    stop("invalid entry: 'datetime' must be POSIXct or POSIXlt")
  
  if (length(level) != length(datetime))
    stop("invalid entry: 'datetime' must be POSIXct or POSIXlt")
  
  if (units == "feet") {
    level <- level * 0.3048
  } else if (units != "meters") {
    stop("invalid units: must be 'feet' or 'meters'")
  }
  
  if (is.numeric(platform) == FALSE) {
    stop("invalid entry: 'platform' must be a numeric input")
  } else if (is.numeric(platform) == TRUE & units == "feet") {
    platform <- platform * 0.3048
  }
  
  if (is.numeric(frq.dur.inc) == FALSE)
    stop("invalid entry: 'frq.dur.inc' must be numeric")
  if (units == "feet") {
    frq.dur.inc <- frq.dur.inc * 0.3048
  }
  
  if (is.numeric(TV.inc) == FALSE)
    stop("invalid entry: 'TV.inc' must be numeric")
  if (units == "feet") {
    TV.inc <- TV.inc * 0.3048
  }
  
  t.int <- (as.numeric(datetime[2]) - as.numeric(datetime[1])) / 60
  elevation <- seq(from = -1, to = 0.5, by = frq.dur.inc)
  
  frq.dur.df <- data.frame(elevation)
  

  wl <- data.frame(datetime, level)
  wl <- wl[wl$datetime <= wl$datetime[nrow(wl)] & wl$datetime > (wl$datetime[nrow(wl)] - 60 * 60 * 24 * 365), ]
  wl$zeroed <- wl[, 2] - platform
  high.low <- HL(level = level, time = datetime, period = period, tides = "H")
  
  #   a <- Sys.time()
  for (i in 1:length(elevation)) {
    
    # is elevation flooded? "flooded" includes target elevation
    wl$ind <- ifelse(wl$zeroed >= elevation[i] & !is.na(wl$zeroed), 1, 0) 
    wl.rle <- rle(wl$ind) # calculate run lengths, used in D90 calculations
    
    ### This will change to freq. of flooding HTs 
    nos <- c(0, cumsum(wl$ind[-1] != wl$ind[-length(wl$ind)])) 
    ifelse(wl$ind[1] == 1,  
      frq.dur.df$frequency[i] <- round((max(nos[nos %% 2 == 0]) / 2), 0),  # if data starts out flooded, do this
      frq.dur.df$frequency[i] <- round((max(nos[nos %% 2 != 0]) / 2), 0) # if starts out dry, do this
    )
    
    ###
    
    frq.dur.df$duration[i] <- max(cumsum(wl$ind) ) * t.int / 60
    frq.dur.df$D90[i]      <- as.numeric(stats::quantile(wl.rle$lengths[wl.rle$values == 1], 0.9)) * t.int / 60 
    frq.dur.df$A[i]        <- mean(wl$zeroed[wl$ind == 1], na.rm = T) - elevation[i]  # average of all flooded time points, not average of flooding high tide heights
    frq.dur.df$A.ht[i]     <- mean(high.low$level[high.low$level >= elevation[i]], na.rm = T) - elevation[i]  # mean depth of flooding high tides. inclusive of target elevation
  }
#   del <- Sys.time() - a
#   del
  
  df      <- frq.dur.df[frq.dur.df$elevation == 0, 2:5]
  df$DV   <- (frq.dur.df$duration[frq.dur.df$elevation == paste("-", TV.inc, sep="")] - 
                 frq.dur.df$duration[frq.dur.df$elevation == 0]) / (TV.inc * 100 * 
                 frq.dur.df$duration[frq.dur.df$elevation == 0]) * 100 
  df$D90V <- (frq.dur.df$D90[frq.dur.df$elevation == paste("-", TV.inc, sep="")] - 
                 frq.dur.df$D90[frq.dur.df$elevation == 0]) / (TV.inc * 100 * 
                 frq.dur.df$D90[frq.dur.df$elevation == 0]) * 100
  rownames(df) <- NULL
  
  grDevices::png(filename, height = 200, width = 200, units = "mm", res = 300)
  graphics::par(mfrow = c(2,2))
  graphics::par(mar   = c(4,4.5,0.2,0.2), oma = c(1,0.2,0.2,0.2))
  
  graphics::plot(frq.dur.df$frequency ~ frq.dur.df$elevation, cex = 0.1, col = "black", type = "l",
       xlim = c(-1, 0.6), yaxt = "n", ylim = c(0, max(frq.dur.df$frequency)),
       ylab = expression(F~"(yr"^-1~")"), xlab = "", bty="n", xaxs = "i", yaxs = "i") 
  graphics::axis(2, las = 1)
  
  graphics::plot(frq.dur.df$D90 ~ frq.dur.df$elevation, cex = 0.1, col = "black", type = "l",
       xlim = c(-1, 0.6), ylim = c(1, max(frq.dur.df$D90)), bty = "n",
       ylab = expression(D90~"(hr "~yr^-1~")"), yaxt = "n", log = "y", xlab = "",
       xaxs = "i", yaxs = "i") 
  graphics::axis(2, 10^c(-2:10), labels = 10^c(-2:10), las = 1)  
  
  graphics::plot(frq.dur.df$duration ~ frq.dur.df$elevation, cex = 0.1, col = "black", type = "l",
       xlim = c(-1, 0.6), ylim = c(1, max(frq.dur.df$duration)),
       ylab = expression(D~"(hr "~yr^-1~")"), yaxt = "n", log = "y", xlab = "", 
       bty = "n", xaxs = "i", yaxs = "i") 
  graphics::axis(2, 10^c(-2:10), labels = 10^c(-2:10), las = 1)  
  
  graphics::plot(frq.dur.df$A ~ frq.dur.df$elevation, cex = 0.1, col = "black", type = "l",
       xlim = c(-1, 0.6), ylim = c(0, max(frq.dur.df$A)), yaxt = "n",
       ylab ="A (m)", xlab = "", bty = "n", xaxs = "i", yaxs = "i") 
  graphics::axis(2, las = 1)

  graphics::mtext("Elevation (m)", side = 1, outer = TRUE, cex = 1, line = 0)
  grDevices::dev.off()

print(paste0(filename, " saved to ", getwd()))
value <- list(dataset = frq.dur.df,
                metrics = df)
}
