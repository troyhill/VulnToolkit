#' @title Scrapes harmonic constituent data from NOAA CO-OPS website
#' 
#' @description Scrapes harmonic constituent data from NOAA CO-OPS 
#' website. Requires an internet connection.
#' 
#' @usage harcon(station)
#' 
#' @param station station ID number or vector of IDs, available on CO-OPS 
#' website (\url{https://co-ops.nos.noaa.gov/stations.html?type=Water+Levels}) or 
#' from \code{\link{noaa.stations}}.
#' 
#' @return a dataframe of harmonic constituents and their associated phases, 
#' amplitudes, and speeds.
#' 
#' @seealso \code{\link{noaa.stations}}
#' 
#' @references \url{http://wetlandsandr.wordpress.com/}
#' 
#' @import RCurl
#' @import XML
#' 
#' @examples \dontrun{
#' bport.cons <- harcon(8467150) # Bridgeport, CT
#' bport.cons
#' } 
#' @export


harcon <- function(station) {
  
#  require(RCurl)
#  require(XML)
  
  page <- htmlParse(getURL(paste("https://co-ops.nos.noaa.gov/harcon.html?unit=0&timezone=0&id=", station, sep = "")),
                    useInternalNodes = TRUE)
  
  nodes <- getNodeSet(page, "//td")
  
  nodes.text <- xmlSApply(nodes, xmlValue)
  
  if (length(nodes.text) == 0) { # list length will be zero if no harmonic constituent page exists (e.g., Great Lake stations)
         
         hc.name   <- NA
         hc.desc   <- NA
         hc.amp    <- NA
         hc.phase  <- NA
         hc.speed  <- NA
  } else {
         hc.name   <- as.factor(nodes.text[c(seq(from = 2, to = length(nodes), by = 6))])
         hc.desc   <- as.factor(nodes.text[c(seq(from = 6, to = length(nodes), by = 6))])
         hc.amp    <- as.numeric(nodes.text[c(seq(from = 3, to = length(nodes), by = 6))])
         hc.phase  <- as.numeric(nodes.text[c(seq(from = 4, to = length(nodes), by = 6))])
         hc.speed  <- as.numeric(nodes.text[c(seq(from = 5, to = length(nodes), by = 6))])
  }
         
  output.tmp <- data.frame(hc.desc, hc.name, hc.amp, hc.phase, hc.speed)
  names(output.tmp)[3] <- paste("amplitude.", station, sep = "") 
  names(output.tmp)[4] <- paste("phase.", station, sep = "")
  names(output.tmp)[5] <- paste("speed.", station, sep = "")  
  
  invisible(output.tmp)
}


