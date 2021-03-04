#' @title Calculate tidal form number
#' 
#' @description Uses harmonic constituent data from the NOAA 
#' CO-OPS website to calculate tidal form numbers as the ratio 
#' of the sum of K1 and O1 diurnal harmonic constituent amplitudes 
#' to the sum of the M2 and S2 semidiurnal amplitudes. Requires 
#' an internet connection.
#' 
#' @usage form.no(station)
#' 
#' @param station station ID number or vector of IDs, available on CO-OPS 
#' website (\url{https://www.tidesandcurrents.noaa.gov/stations.html?type=Water+Levels}) or 
#' from \code{\link{noaa.stations}}.
#' 
#' @return a dataframe of station number(s) and corresponding tidal form number(s).
#' 
#' @seealso \code{\link{noaa.stations}}
#' 
#' @examples \dontrun{
#' a <- form.no()
#' stn.list <- c("8467150", "8461490", "9454240")
#' b <- form.no(stn.list)
#' b
#' } 
#' @export

form.no <- function(station = 8467150){
if(length(station) == 1){ 
  h <- harcon(station)
  
  f <- (h[, 3][h$hc.name == "K1"] + h[, 3][h$hc.name == "O1"]) / 
       (h[, 3][h$hc.name == "M2"] + h[, 3][h$hc.name == "S2"])     
  harcon.data <- data.frame(station = station, form.number = f)
} else if(length(station) > 1){
  harcon_list <- lapply(station, function(x) harcon(x))
    
  for(i in 1:length(harcon_list)) {
    tmp <- data.frame(harcon_list[i])
    f <- (tmp[, 3][tmp$hc.name == "K1"] + tmp[, 3][tmp$hc.name == "O1"]) / 
      (tmp[, 3][tmp$hc.name == "M2"] + tmp[, 3][tmp$hc.name == "S2"])     
    harcon.data.tmp <- data.frame(station = station[i], form.number = f)
    
    if (exists("harcon.data")){
      harcon.data <- rbind(harcon.data, harcon.data.tmp)  
    }else if (!exists("harcon.data")){
      harcon.data <- harcon.data.tmp 
    }
  }
}
harcon.data
}
