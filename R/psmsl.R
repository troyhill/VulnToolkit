#' @title Download sea level data from \url{http://www.psmsl.org}
#' 
#' @description
#' \code{psmsl} imports data stewarded by the Permanent Service for Mean Sea Level.
#' 
#' @usage psmsl(station = 12, type = "RLR", interval = "annual") 
#' 
#' @param station name or ID, or a vector of station names/IDs. Elements can be 
#' a character string (must match actual station name identically), or numeric 
#' station ID (no quotes: i.e., 12 rather than "12"). Use \code{\link{psmsl.stations}} 
#' to find stations, or check www.psmsl.org. If multiple stations are included, their 
#' data is combined using rbind, making a long (rather than wide) dataset. Default 
#' station is the Battery, in New York City.
#' @param type data quality class; can be 'metric' (default; data has been rigorously 
#' standardized) or 'RLR'. See www.psmsl.org for documentation.
#' @param interval time interval over which mean sea level is calculated. Can be 
#' 'monthly' or 'annual'.
#' 
#' @return A data frame \code{data.frame} containing the requested Permanent Service for Mean Sea Level data.
#' 
#' @seealso \code{\link{psmsl.stations}} can be used to find stations
#' 
#' @importFrom utils read.table
#' 
#' 
#' @examples
#' \dontrun{
#' battery  <- psmsl()
#' stations <- psmsl(station = c(1372, 12), interval = "monthly")
#' }
#' @export

psmsl <- function(station = 12, type = "RLR", interval = "annual"){ 
  type <- tolower(type)
  if(grepl(x = type, pattern = "rlr")){
    type <- "rlr"
  } else if(grepl(x = type, pattern = "metric")) {
    type <- "met"
  }  else stop("data 'type' requested must be either 'RLR' or 'metric'")
  
  if(interval %in% c("monthly", "annual") == FALSE) {
    stop("'interval' argument must be either 'monthly' or 'annual'")
  }
  
  stn.list <- psmsl.stations() 
  
  for(i in 1:length(station)) {
    if(suppressWarnings(is.na(as.numeric(station[i]))) == FALSE) {
      stn.no <- station[i]
    } else {
      if (grepl(x = toupper(station[i]), pattern = stn.list$name)) {
        stn.no <- stn.list$id[stn.list$name == toupper(station[i])]
      } else stop("station name not found - to verify the name or find the station number, run psmsl.list() or check www.psmsl.org")
    }
    
    url_page <- paste0("https://www.psmsl.org/data/obtaining/", type, ".", interval, ".data/", stn.no, ".", type, "data")
    output <- utils::read.table(file = url_page, sep = ";", col.names = c("year", "msl_mm", "days_missing", "flag"))  
    output$msl_mm[output$msl_mm == -99999] <- NA
    output$site <- stn.list$name[stn.list$id == stn.no]
    output$id   <- stn.list$id[stn.list$id == stn.no]
    
    if (exists("output.data")){
      output.data <- rbind(output.data, output)  
    }
    else if (!exists("output.data")){
      output.data <- output 
    }}
  return(output.data)
}
