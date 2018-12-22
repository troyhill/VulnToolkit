#' @title Extracts high and low tides from a record of water levels
#' 
#' @description Extracts high and low tides from a record of water levels
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
#' @return a dataframe of tide levels, associated time stamps, and tide 
#' type ('H' or 'L').
#' 
#' @seealso \code{\link{HL.plot}}
#' 
#' 
#' @references \url{http://wetlandsandr.wordpress.com/}
#' 
#' @examples data(NL_6min_2013)
#' HL.NL <- VulnToolkit::HL(level = NL_6min_2013[,2], time = NL_6min_2013[,1])
#' head(HL.NL)
#' @export

HL <- function(level, time, period = 13, phantom = TRUE, tides = "all",
               semidiurnal = TRUE) {

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

# Set locals
partial.tide <- period * 60 * 60            # seconds
t.int <- as.numeric( time[2] ) - as.numeric( time[1] )      # seconds
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
      numLowTides  <- sum(!is.na(hl$level[(days == unique(days)[i]) & (hl$tide %in% "L")]))
      numHighTides <- sum(!is.na(hl$level[(days == unique(days)[i]) & (hl$tide %in% "H")]))
      if (numHighTides == 2) {
        # find higher & lower high tides
        hh.tide <- which.max(hl$level[(days == unique(days)[i]) & (hl$tide %in% "H")])
        lh.tide <- which.min(hl$level[(days == unique(days)[i]) & (hl$tide %in% "H")])
        
        # define new tides
        hl[, "tide2"][(days == unique(days)[i]) & (hl$tide %in% "H")][hh.tide] <- "HH"
        hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "H")][lh.tide] <- "LH"
      } else if (numHighTides == 1) {
        ### define value based on previous or subsequent values
        hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "H")] <- rev(hl[,"tide2"][(hl$tide %in% "H") & (!is.na(hl[,"tide2"]))])[2] # copy the second most recent HH/LH value (may bewrong if there's a gap in the hl)
      }
      
      if (numLowTides == 2) {
        hl.tide <- which.max(hl$level[(days == unique(days)[i]) & (hl$tide %in% "L")])
        ll.tide <- which.min(hl$level[(days == unique(days)[i]) & (hl$tide %in% "L")])
        
        hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "L")][hl.tide] <- "HL"
        hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "L")][ll.tide] <- "LL"
      } else if (numLowTides == 1) {
        ### define value based on previous or subsequent values
        hl[,"tide2"][(days == unique(days)[i]) & (hl$tide %in% "L")] <- rev(hl[,"tide2"][(hl$tide %in% "L") & (!is.na(hl[,"tide2"]))])[2]
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



