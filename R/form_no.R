# code calculates tidal form number according to National Ocean Service's Tide and Current Glossary (2000)
# ratio of the sum of the two largest diurnal amplitudes to the sum of the two largest semidiurnal amplitudes
# required arguments:
# station: station number, or a vector of station numbers
#
# output: a dataframe with station number(s) and corresponding form number(s)

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
