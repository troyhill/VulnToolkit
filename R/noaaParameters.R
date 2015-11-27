noaa.parameters <- function(stn = 8518750) {
  # stn = NOAA station of interest
  # this function returns a cleaned up version of the parameter list page for a site
  # it's needed for the meteorological data in noaa() because the parameter on 
  # the first line is consistently missed during an XML conversion.
  if (!regexpr("[0-9]{7}", stn)[1] == 1) {
     stop("Invalid station entry: must use station number. Check active stations 
   at: http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels")
  }
  
  allParams <- data.frame(params = as.character(NA), startDate = NA, endDate = NA, station = stn)
  tempDoc      <- htmlParse(getURL(paste0("http://co-ops.nos.noaa.gov/inventory.html?id=", stn)),
                            useInternalNodes = TRUE)
  TempNodes    <- getNodeSet(tempDoc, "//tr")
  for (i in 2:length(TempNodes)) {
    out2   <- xpathSApply(tempDoc, "//tr", saveXML)[i] # converts to char vector
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