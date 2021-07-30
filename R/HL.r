#' @title Extracts high and low tides from a record of water levels
#' 
#' @description Extracts high and low tides from a record of water levels. For reliable results, the time interval between samples must be constant throughout the dataset (e.g., 1 sample per hour, 1 sample per 6 minutes). Datasets with irregularly-spaced observations should be expanded to the smallest time interval to avoid issues.
#' 
#' @param level a numeric vector of water levels
#' @param time a vector (numeric or POSIX*) indicating the time of water 
#' level measurements. Units must be minutes.
#' @param period a single numeric or integer estimate of tidal period 
#' (full tidal cycle). Units must be hours.
#' @param phantom a protective measure taken to prevent the inclusion of an 
#' artificial ('phantom') high or low tide at the end of the dataset, which occurs because the final observation is always a local maxima/minima for the period following the previous low/high tide. If the water level 
#' measurements end precisely at a low or high tide, this can be changed to FALSE.
#' @param tides is used to optionally subset the output to include 
#' only high or low tides. This argument can be 'all' (default), 'H', or 'L'
#' @param semidiurnal logical. If TRUE, higher- and lower- high/low tides are reported in a separate column called 'tide2'. Tides are grouped by day and assigned higher- or lower- status. When a single high or low tide exists in a day, tides on adjacent days are used to make an assignment. 
#' @param verbose logical. If TRUE, messages are returned to console identifying the presence of NAs in the input data
#' 
#' @return a dataframe of tide levels, associated time stamps, and tide 
#' type ('H' or 'L'). If there are NAs present in the water level or time datasets, a message reports this information in the console but the NAs are not removed or otherwise acted upon. The column indicating semidiurnal tides (`tide2`) may be NA if tides are identical or in cases where there is a single tide in the first day of the dataset.
#' 
#' @seealso \code{\link{HL.plot}}
#' 
#' 
#' 
#' @examples
#' HL.NL <- VulnToolkit::HL(level = NL_6min_2013[,2], time = NL_6min_2013[,1])
#' head(HL.NL)
#' 
#' \dontrun{
#' ### Important note: Problems arise when using irregularly-spaced data.
#' ### The best solution is for the user to ensure their data are evenly-spaced.
#' ### An example:
#' ### 
#' ### pull data at 6-minute intervals
#' dat.hr   <- noaa(begindate = "20200101", enddate = "20200630", station = "8467150",
#'                  interval = "hourly")
#' 
#' ### pull data at 1-hr intervals
#' dat.6min <- noaa(begindate = "20200701", enddate = "20201231", station = "8467150",
#'                  interval = "6 minute")
#' 
#' dat.combined <- rbind(dat.hr, dat.6min)
#' ### if time interval is inconsistent the HL output can be unreliable
#' HL.plot(level = dat.combined[, 2], time = dat.combined[, 1]) 
#' 
#' ### expand 1-hr dataset using the 6-minute time interval
#' timeSeq <- data.frame(time_GMT = seq.POSIXt(from = min(dat.hr$time_GMT), 
#'                                             to = max(dat.hr$time_GMT), 
#'                                             by = "6 min"))
#' ### create consistently-spaced dataset
#' dat.hr2       <- plyr::join_all(list(timeSeq, dat.hr))
#' dat.combined2 <- rbind(dat.hr2, dat.6min)
#' HL.plot(level = dat.combined2[, 2], time = dat.combined2[, 1]) 
#' }
#' 
#' @export

HL <- function(level, time, period = 13, phantom = TRUE, tides = "all",
               semidiurnal = TRUE, verbose = FALSE) {

# Check arguments
if(is.numeric(level) == FALSE) 
  stop("invalid entry: 'level' must be numeric")

if(is.numeric(time) == FALSE & class(time)[1] != "POSIXlt" & class(time)[1] != "POSIXct" )
  stop("invalid entry: 'time' must be numeric or POSIX*")

if(is.numeric(period) == FALSE & class(period) != "integer") 
  stop("invalid entry: 'period' must be numeric")

if(tides != "all" & tides != "L" & tides != "H") 
  stop("invalid entry: 'tides' must be 'all', 'H', or 'L'")
if(!is.logical(semidiurnal)) {
  stop("'semidiurnal' argument should be TRUE/FALSE - indicates whether higher- and lower- high/low tides should be identified. /n")
}

  if (verbose){
    ### send a message about NAs, if any are present
    if (sum(is.na(time)) > 0) {
      message(sum(is.na(time)), " NAs appear in time data. These have not been removed or modified.\n")
    }
    if (sum(is.na(level)) > 0) {
      message(sum(is.na(level)), " NAs appear in water level data. These have not been removed or modified.\n")
    }
  }
  
# Set locals
partial.tide <- period * 60 * 60            # seconds
t.int <- as.numeric( time[!is.na(time)][2] ) - as.numeric( time[!is.na(time)][1] )      # seconds
wll.2 <- data.frame(1:length(level), level, time)
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
  
  hl <- rbind(ht, lt)
  hl <- hl[order(hl[,1]),]
  hl <- hl[,-1]
  
  if(semidiurnal) {
    ### identify higher- and lower- high/low tides
    days   <- substr(hl$time, 1, 10) # weak point - assumes YYYY-MM-DD format
    hl[, "tide2"] <- NA
    
    for (i in 1:length(unique(days))) {
      ### how many high/low tides are in day i
      ### better approach: do this by tidal cycle (using number.tides, but pairing sequential tides), not by day
      numLowTides  <- sum(!is.na(hl$level[(days == unique(days)[i]) & (hl$tide %in% "L")]))
      numHighTides <- sum(!is.na(hl$level[(days == unique(days)[i]) & (hl$tide %in% "H")]))
      if (numHighTides == 2) {
        # find higher & lower high tides
        hh.tide <- which.max(hl$level[(days == unique(days)[i]) & (hl$tide %in% "H")])
        lh.tide <- which.min(hl$level[(days == unique(days)[i]) & (hl$tide %in% "H")])
        
        ### if tides are exactly equal, the first observation registers as the max and the min
        if (hh.tide == lh.tide) {
          lh.tide <- c(2, 1)[hh.tide] # select the opposite value
        }
        
        # define new tides
        hl[, "tide2"][(days == unique(days)[i]) & (hl$tide %in% "H")][hh.tide] <- "HH"
        hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "H")][lh.tide] <- "LH"
      } else if (numHighTides == 1) {
        ### define value based on previous or subsequent values
        ### this will result in an NA if the first day in the dataset has 1 high/low tide (nothing to base an assessment on)
        hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "H")] <- rev(hl[,"tide2"][(hl$tide %in% "H") & (!is.na(hl[,"tide2"]))])[2] # copy the second most recent HH/LH value (may bewrong if there's a gap in the hl)
        ### if the value is still an NA (can happen on the first day of a dataset), look forward more explicitly
        if(is.na(hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "H")])) {
          if (!is.na(unique(days)[c(i+1)])) { # if there's a 'next day' in the dataset,
            nextDaysHH <- which.max(hl[,"level"][(days %in% unique(days)[c(i+1)]) & (hl$tide %in% "H")]) # look at the next day's high tides: is highest high first or second?
            if (nextDaysHH == 2) {
              hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "H")] <- "HH" 
            } else if (nextDaysHH == 1) {
              hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "H")] <- "LH"
            }
          }
        }
      }
      
      if (numLowTides == 2) {
        hl.tide <- which.max(hl$level[(days == unique(days)[i]) & (hl$tide %in% "L")])
        ll.tide <- which.min(hl$level[(days == unique(days)[i]) & (hl$tide %in% "L")])
        
        ### if tides are exactly equal, the first observation registers as the max and the min
        if (hl.tide == ll.tide) {
          ll.tide <- c(2, 1)[hl.tide] # select the opposite value
        }
        
        hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "L")][hl.tide] <- "HL"
        hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "L")][ll.tide] <- "LL"
      } else if (numLowTides == 1) {
        ### define value based on previous or subsequent values
        hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "L")] <- rev(hl[,"tide2"][(hl$tide %in% "L") & (!is.na(hl[,"tide2"]))])[2]
        if(is.na(hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "L")])) {
          if (!is.na(unique(days)[c(i+1)])) { # if there's a 'next day' in the dataset,
            nextDaysHL <- which.max(hl[,"level"][(days %in% unique(days)[c(i+1)]) & (hl$tide %in% "L")]) # look at the next day's high tides: is highest high first or second?
            if (nextDaysHL == 2) {
              hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "L")] <- "HL" 
            } else if (nextDaysHL == 1) {
              hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "L")] <- "LL"
            }
          }
        }
      }
    }
  }

  ### subset high or low tides if desired
  if(tides == "H") {
    hl <- hl[hl$tide == "H", ]
  } else if(tides == "L") {
    hl <- hl[hl$tide == "L", ]  
  } 

  ### remove phantom tide if desired
  if(phantom == TRUE) {
    hl <- hl[-nrow(hl), ]
  } else if (!is.logical(phantom)) {
    stop("invalid entry: 'phantom' must be 'TRUE' or 'FALSE' ")
  }

  rownames(hl) <- seq(length=nrow(hl))  # re-numbers rows, mostly for aesthetics
  invisible(hl)
}



