#' @title Scrapes elevation datums from NOAA CO-OPS website
#' @description Scrapes elevation datums from NOAA CO-OPS website
#' 
#' @details Scrapes elevation datums from NOAA CO-OPS website. 
#' Requires an internet connection. 
#' 
#' @usage noaa.datums(station = 8467150)
#' 
#' @param station station ID number, available on the CO-OPS website.
#' 
#' @return a dataframe of vertical datum names and their elevations in meters 
#' relative to the station datum for the 1983-2001 epoch. Also contains a column 
#' of times associated with relevant datums (record maximum and minimums, lowest 
#' and highest astronomical tides).
#' 
#' @seealso \code{\link{noaa}}
#' 
#' @importFrom XML getNodeSet
#' @importFrom XML xmlSApply
#' @importFrom XML xmlValue
#' @importFrom XML htmlParse
#' 
#' @examples
#' \dontrun{
#' # examples require internet connection
#' bport.datums <- noaa.datums() # Bridgeport, CT
#' battery.datums <- noaa.datums(station = 8518750) # Battery, NYC
#' }
#' @export

noaa.datums <- function(station = 8467150) {
  
#  require(RCurl)
#  require(XML)
  
  page <- XML::htmlParse(readLines(paste("https://tidesandcurrents.noaa.gov/datums.html?id=", station, sep = ""), warn=FALSE),
                    useInternalNodes = TRUE)
  
  nodes <- XML::getNodeSet(page, "//td")
  
  nodes.text <- XML::xmlSApply(nodes, XML::xmlValue)
  
  datum           <- as.factor(nodes.text[c(seq(from = 1, to = 46, by = 3), 52, 58, 64)])
  description     <- as.character(nodes.text[c(seq(from = 3, to = 48, by = 3), 54, 60, 66)])
  m.STND          <- as.numeric(nodes.text[c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 
                                         41, 44, 47, 53, 59, 65)])
  datetime        <- as.POSIXct(rep(NA, times= 19), format = "%m/%d/%Y %H:%M")
  datetime[16:19] <- as.POSIXct(nodes.text[c(50, 56, 62, 68)], format = "%m/%d/%Y %H:%M")
  
  output.tmp <- data.frame(datum, description, m.STND, datetime)
  names(output.tmp)[1] <- "datum"
  names(output.tmp)[3] <- paste("m.STND", station, sep = "")  
  names(output.tmp)[4] <- paste("time.", station, sep = "")  
    
  invisible(output.tmp)
}

