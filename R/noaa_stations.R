#' @title Print active NOAA stations
#' @description \code{noaa.stations} returns active NOAA CO-OPS tide stations.
#' 
#' @details This function returns a dataframe showing all active stations in 
#' the NOAA CO-OPS network. This can be used to identify stations for use 
#' in \code{noaa}. 
#' 
#' @usage noaa.stations(state = "all")
#' 
#' @param state limits printed records to a state or states of interest. Default is to
#' show 'all' stations.
#' @return A \code{data.frame} with station names, states/territories, and station 
#' numbers. If a 'state' argument is provided that is not found on the NOAA site, an 
#' error message is displayed. If a vector of states is provided, some of which are 
#' invalid, a dataframe will be produced for the valid criteria and invalid criteria 
#' will be noted.
#' 
#' @seealso NOAA station listing: \url{https://www.tidesandcurrents.noaa.gov/stations.html?type=Water+Levels}
#' Historic stations which can also be accessed by \code{noaa}: 
#' \url{http://co-ops.nos.noaa.gov/stations.html}
#' 
#' @examples
#' \dontrun{
#' # examples require internet connection
#' a <- noaa.stations() # all active stations
#' b <- noaa.stations(state = "MA") 
#' c <- noaa.stations(state = c("RI", "CT"))
#' d <- noaa.stations(state = c("OR", "RI", "MA", "Germany", "Pluto")) # two erroneous entries 
#' highlow <- noaa(station = d[1,1], begindate = 20130101)
#' }
#' @export


noaa.stations <- function(state = "all") {
  suppressWarnings(stns <- readLines("https://www.tidesandcurrents.noaa.gov/stations.html?type=Water+Levels")) 
  stn2           <- regexpr("[0-9]{7} .*</a>$", stns[grep("[0-9]{7} ", stns)])
  stn3           <- regmatches(stns[grep("[0-9]{7} ", stns)], stn2)
  site.no        <- gsub(" .*</a>", "", stn3)
  site.name      <- gsub("[0-9]{7} |</a>", "", stn3)
  site.name.only <- gsub(", .*|", "", site.name)
  site.state     <- gsub(".*, ", "", site.name)
  
  output <- data.frame(name = site.name.only, state = site.state, number = site.no, stringsAsFactors = FALSE)
  
  if(length(state) == 0) {
    stop("'state' is a required argument")
  } else if(length(state) == 1){
    if(state == "all"){
      output <- output
    } else if(state %in% output$state){
      output <- output[output$state %in% state, ]
    } else if(!state %in% output$state){
      stop("no match found for state/location filter")
    }
  } else if(length(state) > 1){
    if(length(state[state %in% c(output$state, "all")]) == length(state)){
      output    <- output[output$state %in% state, ]
    } else if(length(state[state %in% c(output$state, "all")]) < length(state)) {
      not.found <- state[!state %in% c(output$state, "all")]
      output    <- output[output$state %in% state[state %in% c(output$state, "all")], ]
      print("stations not found for requested location(s):")
      print(not.found)
    }
  }
  rownames(output) <- 1:nrow(output)
  return(output)
}
