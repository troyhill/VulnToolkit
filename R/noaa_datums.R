### Input argument is a single NOAA station number. See http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels
### Output is a dataframe with a row for each elevation datum listed for the station.
### Results are in meters and calculated over the 1983-2001 epoch.
###
### Code dependencies: RCurl and XML packages. To install: install.packages(c("RCurl", "XML"))
###
### Have a problem? Find a bug? Email Hill.Troy@gmail.com


noaa_datums <- function(station) {
  
#  require(RCurl)
#  require(XML)
  
  page <- htmlParse(getURL(paste("http://co-ops.nos.noaa.gov/datums.html?units=1&epoch=0&id=", station, sep = "")),
                    useInternalNodes = TRUE)
  
  nodes <- getNodeSet(page, "//td")
  
  nodes.text <- xmlSApply(nodes, xmlValue)
  
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

