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
#' for sorting: 'name', 'id', 'lat', 'long', 'country', 'date', 
#' 'coastline', or \code{stn}
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
#' stn.df3 <- psmsl.stations(type = "metric")
#' }
#' @export

psmsl.stations <- function(type = "RLR", country = "all", sort.by = "country") {
#  require(XML)
  type <- toupper(type)
  if(grepl(pattern = "RLR", x = type)) {
    url_page <- "https://www.psmsl.org/data/obtaining/"
  } 
  if(grepl(pattern = "METRIC", x = type)) {
    url_page <- "https://www.psmsl.org/data/obtaining/metric.php"
  }
  if(type %in% c("RLR", "METRIC") == FALSE ) {
    stop("invalid data type: 'type' must be 'RLR' or 'metric' ")
  }
  if(sort.by %in% c("name", "id", "lat", "long", "country", "date", "coastline", 
                    "stn") == FALSE) {
    stop("invalid sorting criterion: 'sort.by' must be 'name', 'id', 'lat', 'long', 'country', 'date', 
                  'coastline', or 'stn' ")
  }
  
  ### pull table
  page       <- XML::htmlParse(readLines(url_page, warn=FALSE),
                               useInternalNodes = TRUE)
  nodes      <- XML::getNodeSet(page, "//td")
  nodes.text <- XML::xmlSApply(nodes, XML::xmlValue)
  
  idx <- grepl("[A-Za-z]", nodes.text)
  # head(nodes.text[idx]) 
  ### sometimes the 'gloss ID' column is empty, so we'll cut it out completely 
  ### and take 3 columns after each element with character elements
  
  toGetOdds  <- which(idx)[c(TRUE, FALSE)]
  toGetEvens <- which(idx)[c(FALSE, TRUE)]
  
  df1 <- nodes.text[sort(c(toGetOdds, toGetOdds + 1, toGetOdds + 2, toGetOdds + 3))]
  df2 <- nodes.text[sort(c(toGetEvens, toGetEvens + 1, toGetEvens + 2, toGetEvens + 3))]
  
  df1 <- as.data.frame(matrix(df1, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
  df2 <- as.data.frame(matrix(df2, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
  
  # 5) combine/put them back
  df.out <- cbind(df1, df2)
  names(df.out)    <- c("name", "id", "lat", "long", "country", "date", "coastline", "stn")
  df.out$id        <- as.numeric(df.out$id)
  df.out$lat       <- as.numeric(df.out$lat)
  df.out$long      <- as.numeric(df.out$long)
  df.out$coastline <- as.numeric(df.out$coastline)
  df.out$stn       <- as.numeric(df.out$stn)
  df.out$date      <- as.POSIXct(x = df.out$date, format = "%d/%m/%Y")
  
  # names(test) <- c("name", "ID", "lat", "long", "GLOSS_ID", "country", "date", "coastline", "number")
  
  # apply country filter & more error checking
  c <- nchar(country)
  if(c > 3) stop("'country' argument must be bewteen 1 and 3 letters")
  for(i in 1:c){
    if(substr(country, i, i) %in% c(letters, toupper(letters)) == FALSE) stop("'country' argument must only contain letters") 
  }
  
  country <- toupper(country)
  test2 <- substr(c(df.out$country, "ALL"), 1, c)
  if(country %in% test2 == FALSE) {
    stop("'country' filter is not valid; no countries have the specified code")
  } else if(country == "ALL") {
    df.out <- df.out
  } else df.out <-  df.out[substr(df.out$country, 1, c) == country, ] 
  
 # sort by stated criterion
  s <- paste0("df.out[, '", sort.by, "']")  
  df.out <- df.out[order(eval(parse(text=s))), ]
  rownames(df.out) <- 1:nrow(df.out)
  return(df.out)
}
