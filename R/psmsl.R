# function brings MSL data from www.psmsl.org into R
# arguments:
# station: station or vector of stations to download data for. Can be a character string (must match actual station name identically), or numeric station ID (no quotes: i.e., 12 rather than "12"); use psmsl.list to find stations, or check www.psmsl.org
# if multiple stations are included, their data is combined column-wise
# type: "RLR" or 'metric'
# interval: "annual" or "monthly"



psmsl <- function(station = 12, type = "RLR", interval = "annual"){  
  if(type == "RLR"){
    type <- "rlr"
  } else if(type == "metric") {
    type <- "met"
  }  else stop("data 'type' requested must be either 'RLR' or 'metric'")
  
  if(interval %in% c("monthly", "annual") == FALSE) {
    stop("'interval' argument must be either 'monthly' or 'annual'")
  }
  
  stn.list <- psmsl.list() 
  
  for(i in 1:length(station)) {
    if(is.na(as.numeric(station[i])) == FALSE) {
      stn.no <- station[i]
    } else {
      if(toupper(station[i]) %in% stn.list$name == TRUE) {
        stn.no <- stn.list$ID[stn.list$name == toupper(station[i])]
      } else if(toupper(station[i]) %in% stn.list$name == FALSE) stop("station name not found - to verify the name or find the station number, run psmsl.list() or check www.psmsl.org")
    }
    
    url <- paste0("http://www.psmsl.org/data/obtaining/", type, ".", interval, ".data/", stn.no, ".", type, "data")
    output <- read.table(file = url, sep = ";", col.names = c("year", "msl_mm", "days_missing", "flag"))  
    output$msl_mm[output$msl_mm == -99999] <- NA
    output$site <- stn.list$name[stn.list$ID == stn.no]
    
    if (exists("output.data")){
      output.data <- rbind(output.data, output)  
    }
    else if (!exists("output.data")){
      output.data <- output 
    }}
  return(output.data)
}