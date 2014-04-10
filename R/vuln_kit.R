### Code to transform tidal data to a set of resources to evaluate vulnerability to sea level rise
### 
### Required arguments: 
###     level: A numeric vector of water levels.
###     datetime: A POSIX* vector of time stamps that correspond to the measurements in 'level'.
###
### Optional arguments: 
###      HL.calc: Determines whether high and low tides will be calculated from the 'level' dataset ('calculated'), 
###         or provided by the user ('user'). Defaults to 'calculated'. If set to 'user', the 'HL.input' argument must be used to
###         define the high/low dataset.
###
###      HL.input: High/low dataset, defaults to calculating this from the input dataset ('calculated'). Alternative entry, 'user', requires
###         that a dataset of H/L tides be supplied by the 'HL.input' argument. User-supplied tide data must 
###         be an R dataframe of the form 'data.frame(datetime2, level2, tide)'. 'level2' data 
###         must be a numeric vector with units of meters (feet are not accepted), corresponding timestamps in "datetime2" must be POSIXct or POSIXlt, 
###         and "tide" is limited to containing the following levels: 'H', 'HH', 'L', and 'LL'.
###
###      HL.output: Determines whether dataset of high and low tides is included in output. Default is 'n';  
###         change to 'y' to include high/low data in output.
###
###      regime: Sets tidal period. 'semi-diurnal' is default and includes mixed tides; 'diurnal' is alternative.
###
###      units: 'meters' is default; 'feet' is alternative. If units are "feet," data is converted internally and output in meters.
###
###      frq.dur.inc: Elevation interval used to calculate flooding frequencies, durations, and wet/dry periods.
###         Defaults to 0.005 m. Units must correspond to 'units' argument.
###
###      TV.inc: Elevation interval used to calculate vulnerability metrics (TFV, TDV, WPV, DPV). 
###         Defaults to 0.1 m. Units must correspond to 'units' argument.
###
###      MHW, MLW, MN: Mean high water, mean low water, and mean tidal range. By default ('calculated'), 
###         these are calculated from the 'level' dataset. If specified by the user, these values should be numeric, 
###         in the same units specified by 'units' argument, and must be relative to the same vertical datum as
###         the 'level' and 'HL.input' datasets.
###
###     
###      
###
### Have a problem, find a bug, or want to be notified of updates? Email Hill.Troy@gmail.com

# Also, can you update the other code to calculate the 4 final flooding parameters (including elevation-flooding curves) 
# and the 2 vulnerability parameters?  I would take out the calculation of MHW (since other than calculating that, you 
# don't need the hi/lo data, and the best use of this code anyway would be to enter a known platform elevation).  Attached is one site you can use to check.


# Before function can be used, 
# load HL() from https://gist.github.com/troyhill/5489261



vuln.kit <- function(level, datetime, HL.calc = "calculated", HL.input = "NA", HL.output = "n", 
                     regime = "semi-diurnal", 
                     units = "meters", frq.dur.inc = 0.005, TV.inc = 0.1, 
                     MHW = "calculated", MLW = "calculated", MN = "calculated") {

if(is.numeric(level) == FALSE)
  stop("invalid entry: 'level' must be numeric")

if(class(datetime)[1] != "POSIXlt" & class(datetime)[1] != "POSIXct" )
  stop("invalid entry: 'datetime' must be POSIXct or POSIXlt")

if(length(level) != length(datetime))
  stop("invalid entry: 'datetime' must be POSIXct or POSIXlt")

if(HL.calc != "calculated" & HL.calc != "user")
  stop("invalid entry: 'HL.calc' must be set to 'calculated' (determined by vuln.kit from the high-frequency data) or 'user' (if user provides a dataset of high and low tides)")

###############
#if(HL.calc == "user" & HL.input == "NA" ) {
#  stop("invalid entry: if 'HL.calc' is set to 'calculated', high/low dataset is calculated internally and HL.input must be 'NA'. 
#       Conversely, if 'HL.calc' is set to 'user', the user must use 'HL.input' to specify the dataset of high and low tides")
#}

#if(HL.calc == "calculated" & HL.input != "NA") { 
#  stop("invalid entry: if 'HL.calc' is set to 'calculated', high/low dataset is calculated internally and HL.input must be 'NA'. 
#       Conversely, if 'HL.calc' is set to 'user', the user must use 'HL.input' to specify the dataset of high and low tides")
#}
###############

if(HL.output != "n" & HL.output != "y" & HL.output != "N" & HL.output != "Y") {
  stop("invalid entry: 'HL.output' must be set to 'y' or 'n'")
}

if(regime == "semi-diurnal" | regime == "mixed") {
    period <- 12    
} else if(regime == "diurnal") {
    period <- 24
} else stop("invalid tidal regime entry: 'regime' must be 'semi-diurnal' or 'diurnal'")

if(units == "feet") {
  level <- level * 0.3048
} else if(units != "meters") {                               # this should perhaps be an else statement, not else if
  stop("invalid units: must be 'feet' or 'meters'")
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

if(MHW != "calculated" & is.numeric(MHW) == FALSE) {
  stop("invalid entry: if not calculated internally, 'MHW' must be a numeric input")
} else if(is.numeric(MHW) == TRUE & units == "feet") {
  MHW <- MHW * 0.3048
}

if(MLW != "calculated" & is.numeric(MLW) == FALSE) {
  stop("invalid entry: if not calculated internally, 'MLW' must be a numeric input")
} else if(is.numeric(MLW) == TRUE & units == "feet") {
  MLW <- MLW * 0.3048
}

if(MN != "calculated" & is.numeric(MN) == FALSE) {
  stop("invalid entry: if not calculated internally, 'MN' must be a numeric input")
} else if(is.numeric(MN) == TRUE & units == "feet") {
  MN <- MN * 0.3048
}


# [ 1) timestep is determined internally in HL() ]
# 2) extract H/L data, or check validity of user-defined data
#   remaining issue: verify that user-defined tide types fall into c("H", "HH", "L", "LL")
#   if(levels(HL.input[,3]) %in% c("H", "HH", "L", "LL") ) {
#   stop("invalid entry: user-defined high/low dataset must have tides indicated as 'H', 'HH', 'L', and/or 'LL'")

if(HL.calc == "calculated") {              # correct "time" to be "datetime"
  partial.tide <- period * 60 * 60                            # seconds
  t.int <- as.numeric( datetime[2] ) - as.numeric( datetime[1] )      # seconds
  wll.2 <- data.frame(1:length(level), level, datetime)
  width <- partial.tide / t.int
  
  # Find high tides
  t.x <- which.max(level[1:(1 + width)])
  
  for(i in 1:length(level)) {
    if(!is.na(t.x[i] + 0.5 * width)) {
      temp <- which.max(level[(t.x[i] + 0.5 * width) : (t.x[i] + 1.25 * width)])
      val <- temp - 1 + (t.x[i] + 0.5 * width)
      t.x <- rbind(t.x, val)
    } else t.x <- t.x 
  }
  ht <- wll.2[t.x[,1], ]          # filters high tides from initial dataset, including dates/times
  ht$tide <- rep("H", nrow(ht))

  # Repeat process for low tides
  t.y <- which.min(level[1:(1 + width)])
  for(i in 1:length(level)) {
    if(!is.na(t.y[i] + 0.5 * width)) {
      temp <- which.min(level[(t.y[i] + 0.5 * width) : (t.y[i] + 1.25 * width)])
      val <- temp - 1 + (t.y[i] + 0.5 * width)
      t.y <- rbind(t.y, val)
    } else t.y <- t.y 
  }
  lt <- wll.2[t.y[,1], ]          
  lt$tide <- rep("L", nrow(lt))
  
  # combine high and low tides
  HL.data <- rbind(ht, lt)
  HL.data <- HL.data[order(HL.data[,1]),]    
  HL.data <- HL.data[,-1]               
  rownames(HL.data) <- seq(length=nrow(HL.data))  
  HL.data <- HL.data[-nrow(HL.data), ]
  
} else if(HL.calc == "user") {
  if(class(HL.input[,1])[1] != "POSIXlt" & class(HL.input[,1])[1] != "POSIXct" ) {
    stop("invalid entry: user-defined high/low dataset must have POSIX* timestamps. see ?as.POSIXct")
  } else if(is.numeric(HL.input[,2]) == FALSE ){
    stop("invalid entry: user-defined high/low dataset contains non-numeric water level data")
  } else HL.data <- HL.input; HL.data[HL.data[,3] == "HH",3] <- "H"; HL.data[HL.data[,3] == "LL",3] <- "L"                      ### debug: what happens if these levels aren't present?
    names(HL.data) <- c("datetime", "level", "tide") 
  } else stop("invalid entry: 'HL.calc' input must be either calculated internally ('calculated') or supplied by the user ('user')") 

if(HL.calc == "user" & units == "feet" )
  HL.input[,2] <- HL.input[,2] * 0.3048
   
   
# 3) determine tidal datums, if calculated internally
if(MHW == "calculated") {
  MHW <- mean(HL.data$level[HL.data$tide == "H"])
} else (MHW == MHW)
if(MLW == "calculated") {
  MLW <- mean(HL.data$level[HL.data$tide == "L"])
} else (MLW == MLW)
if(MN == "calculated") {
  MN <- MHW - MLW
} else (MN == MN)


# 4) make water levels relative to MHW
mhw.level <- level - MHW   # high-frequency data
HL.data$level.mhw <- HL.data$level - MHW


# 5) frequency and duration at 5 mm increments (or user-defined interval) 
elevs <- seq(from = -0.5, to = 0.5, by = frq.dur.inc)
frq.ecdf <- ecdf(HL.data$level.mhw[HL.data$tide == "H"])
dur.ecdf <- ecdf(mhw.level)
durs <- 1 - dur.ecdf(elevs)
frqs <- 1 - frq.ecdf(elevs)


frq.dur.df <- data.frame(elevs, frqs, durs)

par(mfrow = c(1,2))
par(mar = c(4,4,0.2,0.2))

plot(frqs ~ elevs, cex = 0.1, col = "black", type = "l",
                 xlim = c(-0.5, 0.5), yaxt="n", ylim = c(0, 1 - frq.ecdf(-0.5)),
                 ylab ="", xlab = paste("Elevation (m; MHW)", sep = "")) 
           axis(2, at=axTicks(2), labels=sprintf("%1.0f%%", 100*axTicks(2)), las=1)  
           lines(durs ~ elevs, cex = 0.1, col = "red", type = "l")
           text(x = 0.4, y = 0.6, "frequency")
           text(x = 0.39, y = 0.5, "duration", col = "red")


# 6) calculate TFV, TDV at MHW
TFV <- (frq.dur.df$frqs[frq.dur.df$elevs == 0] - frq.dur.df$frqs[frq.dur.df$elevs == paste("-", TV.inc, sep="")]) / (TV.inc*-100)   # units: % / cm
TDV <- (frq.dur.df$durs[frq.dur.df$elevs == 0] - frq.dur.df$durs[frq.dur.df$elevs == paste("-", TV.inc, sep="")]) / (TV.inc*-100)   # units: % / cm


# 7) restrict data to most recent year-long interval, and calculate longest dry and wet periods by elevation
wl <- data.frame(datetime, level)
wl <- wl[wl$datetime < wl$datetime[nrow(wl)] & wl$datetime > (wl$datetime[nrow(wl)] - 60*60*24*365), ]
frq.dur.df$wet.max <- as.numeric(rep(NA, times = nrow(frq.dur.df)))
frq.dur.df$dry.max <- as.numeric(rep(NA, times = nrow(frq.dur.df)))

for(i in 1:length(elevs)) {
  
  ### creat dummy indicator for a given elevation
  wl$zeroed <- wl[,2] - MHW
  wl$ind <- ifelse(wl[,3] >= elevs[i], 1, 0)  # 1 indicates elevation is flooded
  
  ### exclude 1's and find longest time change/wet period
  wl.sub <- wl[wl$ind != 1,]
  wl.sub$t2 <- c(wl.sub[-1,1], NA)
  wl.sub$dt <- wl.sub$t2 - wl.sub[,1]

  frq.dur.df$wet.max[i] <- max(wl.sub$dt, na.rm = T) / 60
    
  ### dry periods
  wl.sub <- wl[wl$ind != 0,]
  wl.sub$t2 <- c(wl.sub[-1,1], NA)
  wl.sub$dt <- wl.sub$t2 - wl.sub[,1]

  frq.dur.df$dry.max[i] <- max(wl.sub$dt, na.rm = T) / 60
    
}

# 8) plot wet and dry periods vs. elev
plot(frq.dur.df$dry.max ~ frq.dur.df$elevs, cex = 0.1, col = "black", type = "l",
                xlim = c(-0.5, 0.5), yaxt="n", 
                ylim = c(min(frq.dur.df$wet.max), max(frq.dur.df$dry.max)),
                ylab ="Hours", log = "y", xlab = paste("Elevation (m; MHW)", sep = "")) 
axis(2, 10^c(-2:10), labels=10^c(-2:10), las=1)  
lines(frq.dur.df$wet.max ~ frq.dur.df$elevs, cex = 0.1, col = "red", type = "l")
text(x = 0.3, y = 10^1.6, "longest dry period")
text(x = 0.3, y = 10^1.1, "longest wet period", col = "red")




# 9) calculate WPV, DPV
WPV <- (frq.dur.df$wet.max[frq.dur.df$elevs == 0] - frq.dur.df$wet.max[frq.dur.df$elevs == paste("-", TV.inc, sep="")] ) / (TV.inc*-100)   # units: hr / cm
DPV <- (frq.dur.df$dry.max[frq.dur.df$elevs == 0] - frq.dur.df$dry.max[frq.dur.df$elevs == paste("-", TV.inc, sep="")] ) / (TV.inc*-100)   # units: hr / cm




# 10) output 
if(HL.output == "n" | HL.output == "N"){
  output <- list(frq.dur.df = frq.dur.df,                     # dataset with freq, dur, wet/dry times by elevation
               TFV = TFV, TDV = TDV, WPV = WPV, DPV = DPV,    # vulnerability metrics
               MHW = MHW, MLW = MLW, MN = MN                  # tidal datums (in datum of inital data)
  ) 
} else if(HL.output == "y" | HL.output == "Y"){
   output <- list(frq.dur.df = frq.dur.df,                    
               TFV = TFV, TDV = TDV, WPV = WPV, DPV = DPV,    
               MHW = MHW, MLW = MLW, MN = MN,                  
               high.low = HL.data                             # dataset of high and low tides
)}


}
