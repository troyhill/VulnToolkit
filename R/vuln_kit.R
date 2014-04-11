### Code to transform tidal data to a set of resources to evaluate vulnerability to sea level rise
### 
### Required arguments: 
###     level: A numeric vector of water levels.
###
###     datetime: A POSIX* vector of time stamps that correspond to the measurements in 'level'.
###
###     MHW: Mean high water, in the same units specified by 'units' argument and 
###         relative to the same vertical datum as 'level.'
###
###      platform: Elevation of the marsh platform (or another vertical position of interest). Should be
###         in the units specified by 'units' argument and relative to the same vertical datum as 'level.'
###
### Optional arguments: 
###      units: 'meters' is default; 'feet' is alternative. If units are "feet," data is converted internally and output in meters.
###
###      frq.dur.inc: Elevation interval used to calculate flooding frequencies, durations, and wet/dry periods.
###         Defaults to 0.005 m. Units must correspond to 'units' argument.
###
###      TV.inc: Elevation interval used to calculate vulnerability metrics (TFV, TDV, WPV, DPV). 
###         Defaults to 0.1 m. Units must correspond to 'units' argument.


vuln.kit <- function(level, datetime, MHW, platform = MHW, units = "meters", frq.dur.inc = 0.005, 
                     TV.inc = 0.1) {
  
  if(is.numeric(level) == FALSE)
    stop("invalid entry: 'level' must be numeric")
  
  if(class(datetime)[1] != "POSIXlt" & class(datetime)[1] != "POSIXct" )
    stop("invalid entry: 'datetime' must be POSIXct or POSIXlt")
  
  if(length(level) != length(datetime))
    stop("invalid entry: 'datetime' must be POSIXct or POSIXlt")
  
  if(is.numeric(platform) == FALSE)
    stop("invalid entry: 'platform' elevation must be numeric")
  
  if(units == "feet") {
    level <- level * 0.3048
  } else if(units != "meters") {
    stop("invalid units: must be 'feet' or 'meters'")
  }
  
  if(is.numeric(MHW) == FALSE) {
    stop("invalid entry: 'MHW' must be a numeric input")
  } else if(is.numeric(MHW) == TRUE & units == "feet") {
    MHW <- MHW * 0.3048
  }
  
  if(is.numeric(frq.dur.inc) == FALSE)
    stop("invalid entry: 'frq.dur.inc' must be numeric")
  if(units == "feet") {
    frq.dur.inc <- frq.dur.inc * 0.3048
  }
  
  if(is.numeric(TV.inc) == FALSE)
    stop("invalid entry: 'TV.inc' must be numeric")
  if(units == "feet") {
    TV.inc <- TV.inc * 0.3048
  }
  
  t.int <- (as.numeric( datetime[2] ) - as.numeric( datetime[1] ) )  / 60
  elevation <- seq(from = -1, to = 0.5, by = frq.dur.inc)
  
  frq.dur.df <- data.frame(elevation)
  

  wl <- data.frame(datetime, level)
  wl <- wl[wl$datetime <= wl$datetime[nrow(wl)] & wl$datetime >= (wl$datetime[nrow(wl)] - 60*60*24*365), ]
  wl$zeroed <- wl[,2] - MHW - (platform - MHW)
  
  for(i in 1:length(elevation)) {
    
    wl$ind <- ifelse(wl$zeroed > elevation[i] & !is.na(wl$zeroed), 1, 0)
    wl.rle <- rle(wl$ind)
    
    nos <- c(0,cumsum(wl$ind[-1L] != wl$ind[-length(wl$ind)]))   # numbers flooding events
    if(wl$ind[1] == 1) {   # if sequence starts out flooded, no. of flooding events is max of even numbers % 2
      frq.dur.df$frequency[i] <- round((max(nos[nos %% 2 == 0]) / 2), 0)           # F (yr-1)
    } else if(wl$ind[1] == 0) {
      frq.dur.df$frequency[i] <- round((max(nos[nos %% 2 != 0]) / 2), 0)
    }
    
    frq.dur.df$duration[i] <- max(cumsum(wl$ind) ) * t.int / 60 # D (hr/yr)
    frq.dur.df$D90[i]  <- as.numeric(quantile(wl.rle$lengths[wl.rle$values == 1], 0.9)) * t.int / 60  # hr
    frq.dur.df$A[i]    <- mean(wl$zeroed[wl$ind == 1], na.rm = T) - elevation[i]    # m
  }
  
  df <- frq.dur.df[frq.dur.df$elevation == 0, 2:5]
  df$DV   <- ( frq.dur.df$duration[frq.dur.df$elevation == paste("-", TV.inc, sep="")] - frq.dur.df$duration[frq.dur.df$elevation == 0] ) / (TV.inc*100 * frq.dur.df$duration[frq.dur.df$elevation == 0]) * 100   # units: % / cm
  df$D90V <- ( frq.dur.df$D90[frq.dur.df$elevation == paste("-", TV.inc, sep="")] - frq.dur.df$D90[frq.dur.df$elevation == 0] ) / (TV.inc*100 * frq.dur.df$D90[frq.dur.df$elevation == 0]) * 100                  # units: % / cm
  rownames(df) <- NULL
  
  par(mfrow = c(2,2))
  par(mar = c(4,4.5,0.2,0.2))
  
  plot(frq.dur.df$frequency ~ frq.dur.df$elevation, cex = 0.1, col = "black", type = "l",
       xlim = c(-1, 0.6), yaxt = "n", ylim = c(0, max(frq.dur.df$frequency)),
       ylab = expression(F~"(yr"^-1~")"), xlab = "", bty="n", xaxs = "i", yaxs = "i") 
  axis(2, las=1)
  
  plot(frq.dur.df$D90 ~ frq.dur.df$elevation, cex = 0.1, col = "black", type = "l",
       xlim = c(-1, 0.6), ylim = c(1, max(frq.dur.df$D90)), bty="n",
       ylab = expression(D90~"(hr "~yr^-1~")"), yaxt="n", log = "y", xlab = "",
       xaxs = "i", yaxs = "i") 
  axis(2, 10^c(-2:10), labels=10^c(-2:10), las=1)  
  
  plot(frq.dur.df$duration ~ frq.dur.df$elevation, cex = 0.1, col = "black", type = "l",
       xlim = c(-1, 0.6), ylim = c(1, max(frq.dur.df$duration)),
       ylab = expression(D~"(hr "~yr^-1~")"), yaxt="n", log = "y", xlab = "", 
       bty="n", xaxs = "i", yaxs = "i") 
  axis(2, 10^c(-2:10), labels=10^c(-2:10), las=1)  
  
  plot(frq.dur.df$A ~ frq.dur.df$elevation, cex = 0.1, col = "black", type = "l",
       xlim = c(-1, 0.6), ylim = c(0, max(frq.dur.df$A)), yaxt = "n",
       ylab ="A (m)", xlab = "", bty="n", xaxs = "i", yaxs = "i") 
  axis(2, las=1)

value <- list(dataset = frq.dur.df,
                metrics = df)
}
