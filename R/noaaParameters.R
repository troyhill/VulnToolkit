#' @title Reports data available for a NOAA station
#' @description Reports data available for a NOAA station
#' 
#' @details \code{noaa.parameters} reports the parameters (meteorological 
#' and tidal) available for a specified NOAA station. 
#' 
#' 
#' @param stn NOAA station number (note that station names are not 
#' yet acceptable inputs). Default station is Bridgeport, CT.
#' 
#' @return a dataframe with parameters and associated start and end 
#' dates. Where a parameter's availability is not continuous, multiple
#'  rows are reported. This function can be used to, for example, select 
#'  meteorological parameters to include in calls to \code{\link{noaa}}
#' 
#' @seealso \code{\link{noaa}}, \code{\link{noaa.stations}}
#' 
#' @importFrom XML htmlParse
#' @importFrom XML getNodeSet
#' @importFrom XML xpathSApply
#' 
#' @examples
#' \dontrun{
#' # examples require internet connection
#' noaa.parameters()
#' LA.stns <- noaa.stations(state = "LA")
#' noaa.parameters(LA.stns$number[1])
#' }
#' @export

noaa.parameters <- function(stn = 8467150) {
  # stn = NOAA station of interest
  # this function returns a cleaned up version of the parameter list page for a site
  # it's needed for the meteorological data in noaa() because the parameter on 
  # the first line is consistently missed during an XML conversion.
  if (!regexpr("[0-9]{7}", stn)[1] == 1) {
     stop("Invalid station entry: must use station number. Check active stations 
   at: https://www.tidesandcurrents.noaa.gov/stations.html?type=Water+Levels")
  }
  
  allParams <- data.frame(params = as.character(NA), startDate = NA, endDate = NA, station = stn)
  targetURL <- paste0("https://tidesandcurrents.noaa.gov/inventory.html?id=", stn)
  tempDoc      <- XML::htmlParse(readLines(targetURL),
                            useInternalNodes = TRUE)
  TempNodes    <- XML::getNodeSet(tempDoc, "//tr")
  for (i in 2:length(TempNodes)) {
    out2   <- XML::xpathSApply(tempDoc, "//tr", saveXML)[i] # converts to char vector
    # get parameter name
    line   <- strsplit(out2, "/div>")[[1]][2]
    
    pName  <- strsplit(line, "</td>")[[1]][1]
    pStart <- substr(strsplit(line, "bdate=")[[1]][2], 1, 8)
    pEnd   <- substr(strsplit(line, "edate=")[[1]][2], 1, 8)
    tempParams <- data.frame(params = pName, startDate = pStart, 
                             endDate = pEnd, station = stn)
    allParams <- rbind(allParams, tempParams)
  }
  allParams[!is.na(allParams$startDate), ]
}
