#' @title Generates a list of active and historic PSMSL stations 
#' 
#' @description Generates a list of active and historic tide stations 
#' hosted by the Permanent Service for Mean Sea Level (\url{http://www.psmsl.org})
#' 
#' @details A \code{data.frame} containing all Permanent Service for Mean Sea Level 
#' stations meeting country code criterion. Data frame is sorted by the column 
#' specified in argument \code{sort.by}
#' 
#' @usage psmsl.stations(type = "RLR", country = "all", sort.by = "country")
#' 
#' @param type data quality class; can be 'metric' or 'RLR' - see \url{http://www.psmsl.org} 
#' for documentation
#' @param country if desired, the full list of stations can be filtered by up to three 
#' alphabetical characters. Specific country codes can be entered ("USA"), or 
#' abbreviated codes in case the user isn't sure of the country code ("U"; "US"). 
#' Upper case and lower case codes are both acceptable. Default is 'all' stations.
#' @param sort.by the criterion for sorting the final dataframe.  By default, 
#' output is sorted alphabetically by country code. Any column name can be used 
#' for sorting: 'name', 'ID', 'lat', 'long', 'GLOSS_ID', 'country', 'date', 
#' 'coastline', or \code{number}
#' 
#' @return A \code{data.frame} containing all Permanent Service for Mean Sea Level 
#' stations meeting country code criterion. Data frame is sorted by the column specified 
#' in argument \code{sort.by}
#' 
#' @seealso \code{\link{psmsl}}
#' 
#' @import XML
#' 
#' @examples
#' \dontrun{
#' # examples require internet connection
#' stn.df <- psmsl.stations()
#' stn.df2 <- psmsl.stations(country = "USA", sort.by = "date")
#' }

psmsl.stations <- function(type = "RLR", country = "all", sort.by = "country") {
#  require(XML)
  if(type == "RLR") {
    url <- "http://www.psmsl.org/data/obtaining/"
  } else if(type == "metric") {
    url <- "http://www.psmsl.org/data/obtaining/metric.php"
  }
  if(type %in% c("RLR", "metric") == FALSE ) {
    stop("invalid data class: 'class' must be 'RLR' or 'metric' ")
  }
  if(sort.by %in% c("name", "ID", "lat", "long", "GLOSS_ID", "country", "date", "coastline", 
                    "number") == FALSE) {
    stop("invalid sorting criterion: 'sort.by' must be 'name', 'ID', 'lat', 'long', 'GLOSS_ID', 'country', 'date', 
                  'coastline', or 'number' ")
  }
  test <- XML::readHTMLTable(url,
                        colClasses = c("character", "numeric", "numeric", "numeric",
                                       "numeric", "character", "character", "numeric", "numeric"), 
                        as.data.frame = TRUE, stringsAsFactors = FALSE)
  test <- as.data.frame(test[1])
  names(test) <- c("name", "ID", "lat", "long", "GLOSS_ID", "country", "date", "coastline", "number")
  test$date <- as.POSIXct(test$date, format = "%d/%m/%Y")
  
  
  # apply country filter & more error checking
  c <- nchar(country)
  if(c > 3) stop("'country' argument must be bewteen 1 and 3 letters")
  for(i in 1:c){
    if(substr(country, i, i) %in% c(letters, toupper(letters)) == FALSE) stop("'country' argument must only contain letters") 
  }
  
  country <- toupper(country)
  test2 <- substr(c(test$country, "ALL"), 1, c)
  if(country %in% test2 == FALSE) {
    stop("'country' filter is not valid; no countries have the specified code")
  } else if(country == "ALL") {
    test <- test
  } else test <-  test[substr(test$country, 1, c) == country, ] 
  
 # sort by stated criterion
  s <- paste0("test[, '", sort.by, "']")  
  test <- test[order(eval(parse(text=s))), ]
  rownames(test) <- 1:nrow(test)
  return(test)
}
