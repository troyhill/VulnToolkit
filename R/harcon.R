### Input argument is a single NOAA station number. See http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels
### Output is a dataframe with a row for each harmonic constituent, and columns for 
###   each station's amplitude, phase, and speed.
### Output amplitudes are in meters, phases are in degrees, referenced to GMT
###
### Code dependencies: RCurl and XML packages. To install: install.packages(c("RCurl", "XML"))
###
### Have a problem? Find a bug? Email Hill.Troy@gmail.com

harcon <- function(station) {
  
  require(RCurl)
  require(XML)
  
  page <- htmlParse(getURL(paste("http://co-ops.nos.noaa.gov/harcon.html?unit=0&timezone=0&id=", station, sep = "")),
                    useInternalNodes = TRUE)
  
  nodes <- getNodeSet(page, "//td")
  
  nodes.text <- xmlSApply(nodes, xmlValue)
  
  hc.name   <- as.factor(nodes.text[c(seq(from = 2, to = length(nodes), by = 6))])
  hc.desc   <- as.factor(nodes.text[c(seq(from = 6, to = length(nodes), by = 6))])
  hc.amp    <- as.numeric(nodes.text[c(seq(from = 3, to = length(nodes), by = 6))])
  hc.phase  <- as.numeric(nodes.text[c(seq(from = 4, to = length(nodes), by = 6))])
  hc.speed  <- as.numeric(nodes.text[c(seq(from = 5, to = length(nodes), by = 6))])
  
  output.tmp <- data.frame(hc.desc, hc.name, hc.amp, hc.phase, hc.speed)
  names(output.tmp)[3] <- paste("amplitude.", station, sep = "") 
  names(output.tmp)[4] <- paste("phase.", station, sep = "")
  names(output.tmp)[5] <- paste("speed.", station, sep = "")  
  
  invisible(output.tmp)
}


