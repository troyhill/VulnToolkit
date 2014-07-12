# function generates a list of psmsl stations
# arguments:
# 'type' = data quality class; 'metric' or 'RLR'
# 'country' = optional filter; requires correct country code, which isn't always obvious. Can filter by 1- or 2- letter codes. Doesn't currently handle multiple country codes but this is simple - use unique() to remove duplicates.
# 'sort.by' = column to sort table by ("name", "ID", "lat", "long", "GLOSS_ID", "country", "date", "coastline", or "number")
# output: a dataframe listing all PSMSL stations meeting desired criteria and sorted according to 'sort.by'


psmsl.list <- function(type = "RLR", country = "all", sort.by = "country") {
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
  test <- readHTMLTable(url,
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
  return(test)
}
