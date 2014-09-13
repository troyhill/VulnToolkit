#' @title Downloads NOAA CO-OPS tide data
#'
#' @description
#' Scrapes water level data from NOAA CO-OPS website. Requires internet connection.
#'
#' @details
#' @usage /code{noaa(begindate = "begindate", enddate = "enddate", station = "8467150",
#' units = "meters", datum = "MHW", interval = "HL", time = "GMT", continuous = "FALSE")}
#' @param begindate first day of data to download. Format must be YYYYMMDD. If left unspecified, the first complete day of data will be used.
#' enddate final day of data to download. Format must be YYYYMMDD. If left unspecified, the last complete day of data will be used.
#' station station name or ID number, available on the CO-OPS website or by using \code{\link{noaa.stations}}. Entry can be numeric (station ID) or a string corresponding to the station name. Default station is Bridgeport, CT.
#' units can be 'feet' or 'meters'. Default is 'meters'
#' datum vertical reference datum, set to 'MHW' by default. Can be 'station', 'NAVD', 'MLLW', 'MLW', 'MSL', 'MTL', 'MHW', 'MHHW', or 'IGLD' (some datums are not available at some sites)
#' interval sets measurement interval; can be 'HL' (default), '6 minute', 'hourly', or 'monthly'. For data on monthly and annual time scales from Permanent Service for Mean Sea Level, see \code{\link{psmsl}}
#' time can be 'LST', 'GMT', or 'LST/LDT'. Not all time zones are available for all data. GMT appears to have wider availability than LST, so it is the default.
#' continuous determines whether a continuous time series is produced, with lengthy gaps in data filled in with NAs. By default, this is \code{FALSE}. This option only applies to data at evenly spaced intervals (i.e., \code{6 minute} or \code{hourly})
#' @return dataset a dataframe with water levels, associated time stamps, a station ID column, and tide type (if interval is set to \code{HL})
#' @seealso \code{\link{noaa.stations}}
#' @references none
#' @aliases 
#' @keywords 
#' @export
#' @examples
#'# Example requires an internet connection
#'# bport2013 <- noaa(begindate = 20130101, enddate = 20131231, 
#'#     station = "Bridgeport, CT", interval = "6 minute")
#'
#'# test2.1 <- noaa("20100101", "20120101", interval = "hourly") 
#'# test2.2 <- noaa("20100101", "20120101", interval = "hourly", continuous = "TRUE") 
#'# nrow(test2.1) # includes data on NOAA site (incomplete record)
#'# nrow(test2.2) # fills gaps with NAs


noaa <- function(begindate = "begindate", enddate = "enddate", station = "8467150",
                 units = "meters", datum = "MHW", interval = "HL", time = "GMT", continuous = "FALSE") {
  
  if(!continuous %in% c("FALSE", "TRUE", "T", "F")) stop("'continuous' must be set to 'TRUE' or 'FALSE'")
  # set units                                                       
  if(units ==  "meters")       {
    u.csv <- u <- "metric"
  } else if(units ==  "feet") {
    u <- "standard"
    u.csv <- "english"
  }  else stop("invalid units: must be 'feet' or 'meters' ")
  
  
  
  # set datum
  if(datum %in% c("STND", "MHHW", "MHW", "MTL", "MSL", "MLW", "MLLW", "NAVD", "IGLD")){
    datum <- datum  
  } else stop("invalid datum: must be 'STND', 'MHHW', 'MHW', 'MTL',
    'MSL', 'MLW', 'MLLW', 'IGLD', or 'NAVD'")
  
  # set measurement time interval        
  if(interval ==  "6 minute")                   {
    ti.csv <- "water_level"
    ti.name <- "Verified 6-Minute Water Level"
  } else if(interval ==  "hourly")             {
    ti.csv <- "hourly_height"
    ti.name <- "Verified Hourly Height Water Level"
  } else if(interval ==  "HL")                 {
    ti.csv <- "high_low"
    ti.name <- "Verified High/Low Water Level"
  } else if(interval ==  "monthly")                 {
    ti.csv <- "monthly_mean"
    ti.name <- "Verified Monthly Mean Water Level"
  }  else stop("invalid time interval: must be '6 minute', 'hourly', or 'HL'")
  
  # set time zone
  if(time %in%  c("LST/LDT", "GMT", "LST"))     {
    tz <- time
  } else stop("invalid time zone: must be 'LST/LDT', 'GMT', or 'LST' ")
  
  
  # set site name/number indicator
  if(regexpr("[0-9]{7}", station)[1] == 1)      {
    site.ind <- c(1)
  } else if(regexpr("[a-zA-Z]+", station)[1] == 1) {
    site.name <- station
    site.ind <- c(0)
  } else stop("Invalid station entry: must use station name or number. Check active stations 
   at: http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels")
  
  
  
  suppressWarnings(stns <- readLines("http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels")) # list of active stations
  
  if(site.ind == 1) {                                                             # Use station number to identify station
    stn1 <- grep(paste(station, " " , sep = ""), stns)                              # station number is followed by a space, then the station name
    if(length(stn1) == 0) {
      stop("Station number appears to be invalid. No match found at
           http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels")
    } else if(length(stn1) > 1) {
      stop("Station number appears to be duplicated. Try using site name:
           http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels")
    } else if(length(stn1) == 1) {
      stn2 <- regexpr("[0-9] .*</a>$", stns[stn1])
      stn3 <- regmatches(stns[stn1], stn2)                                        # extract matches
      site.name <- gsub("[0-9] |</a>", "", stn3)                                  # clean up site name
    }
  } else if(site.ind == 0) {                                                    # Use station name to identify site number
    no1 <- grep(site.name, stns)                                                    
    if(length(no1) == 1){ 
      no2 <- regexpr("[0-9]{7} .*</a>$", stns[no1])
      no3 <- regmatches(stns[no1], no2)                                       
      station <- site.no <- gsub("[A-Za-z]| |,|</a>", "", no3)                           
    } else if(length(no1) > 1){
      stop("Site name found for multiple active NOAA stations. Look up site number at 
           http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels")
    } else if(length(no1) < 1){
      stop("Site name not found on list of active NOAA stations. Look up sites at 
           http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels. 
           Be attentive to spelling or consider using the station number.")
    }
    }
  
  
  
  
  
  doc <- htmlParse(getURL(paste("http://co-ops.nos.noaa.gov/inventory.html?id=", station, sep="")),
                   useInternalNodes = TRUE)
  nodes <- getNodeSet(doc, "//tr")
  date.list <- sapply(nodes, function(x)  xmlValue(getSibling(x)))
  data.line <- grep(ti.name, date.list)          
  
  first.record <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}[:punct:][0-9]{2}", 
                          date.list[data.line])
  first.rec <- regmatches(date.list[data.line], first.record )
  first.rec <- as.Date(substr(first.rec, 1, 10), format = "%Y-%m-%d")
  last.record <- regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}[:punct:][0-9]{2}$", 
                         date.list[data.line])
  last.rec <- regmatches(date.list[data.line], last.record )
  last.rec <- as.Date(substr(last.rec, 1, 10), format = "%Y-%m-%d")
  
  if(length(date.list[data.line]) > 1) {
    first.rec <- first.rec[1]
    last.rec <- last.rec[length(last.rec)]
  }
  
  
  
  # set start/end dates to full period of record, if left as default
  if(begindate ==  "begindate")        {
    sdate <- strptime(as.character(first.rec), "%Y-%m-%d")
    sdate <- gsub("-", "", sdate)
    sdate <- as.Date(as.character(sdate), "%Y%m%d")              
    sdate <- sdate + 1
  } else if(begindate !=  "begindate")                         {
    sdate <- as.Date(as.character(begindate), "%Y%m%d")
  }
  
  if(enddate ==  "enddate")            {
    edate <- strptime(as.character(last.rec), "%Y-%m-%d")
    edate <- gsub("-", "", edate)             
    edate <- as.Date(as.character(edate), "%Y%m%d")              
    edate <- edate - 1
  } else if(enddate !=  "enddate")                             {
    edate <- as.Date(as.character(enddate), "%Y%m%d")
  }
  
  
  
  # check if date range is within period of record, and check if time period
  # requires splitting into smaller units. Interval limit is 1 year for hourly
  # and HL data,  31 days for 6-min data, 10 years for monthly data.
  dates <- sdate
  if(interval == "HL") { if(sdate < first.rec | edate > last.rec) {
    stop("invalid time interval")
  } else if( as.numeric(edate - sdate) > 364) {
    dates <- seq(sdate,edate, 365)
  } else(dates <- c(sdate,edate))}
  if(interval == "hourly") { if(sdate < first.rec | edate > last.rec) {
    stop("invalid time interval")
  } else if( as.numeric(edate - sdate) > 364) {
    dates <- seq(sdate,edate, 365)
  } else(dates <- c(sdate,edate))}
  if(interval == "6 minute") { if(sdate < first.rec | edate > last.rec) {
    stop("invalid time interval")
  } else if( as.numeric(edate - sdate) > 30)  {
    dates <- seq(sdate,edate, 31)
  } else(dates <- c(sdate,edate))}
  
  if(!edate %in% dates[length(dates)]) dates <- c(dates, edate)
  
  dates2 <- format(as.Date(dates), "%Y%m%d")     # re-format dates for the url
  
#   old version: 
#   url.temp <- c(paste0("http://co-ops.nos.noaa.gov/api/datagetter?", "begin_date=", dates2[i], 
#                        "&end_date=", dates2[i+1], "&station=", station, "&product=", ti.csv, 
#                        "&units=", u.csv, "&time_zone=", tz, "&datum=", datum, 
#                        "&application=Tides_and_Currents","&format=csv"))
#   
  
  # create list of csv files
  for(i in 1:(length(dates2) - 1)) {
    url.temp <- c(paste0("http://tidesandcurrents.noaa.gov/api/datagetter?", 
                         "product=", ti.csv, 
                         "&application=NOS.COOPS.TAC.WL",
                         "&begin_date=", dates2[i], 
                         "&end_date=", dates2[i+1], 
                         "&datum=", datum, 
                         "&station=", station,
                         "&time_zone=", tz, 
                         "&units=", u.csv, 
                         "&format=csv"))
    if (!exists("url.list")){
      url.list <- url.temp
    }
    # if the dataset exists, add to it
    if (exists("url.list")){
      url.list[i] <- url.temp
      rm(url.temp)
    }
  }
  
  
  lapply.csv <- lapply(url.list, function(x) read.csv(x))
  data.csv <- do.call(rbind, lapply.csv)
  data.csv$station <- rep(site.name, times = nrow(data.csv))
  
  label <- paste("verified water level at ", site.name, " (", units, " rel. to ", datum, ")", sep="")
  t.label <- paste("time (", time, ")", sep = "")
  
  
  # clean up the data
  if(interval == "HL" ) {
    data.csv$datetime <- as.POSIXlt(data.csv[, 1], format = "%Y-%m-%d %H:%M")
    data.csv <- data.csv[, c(7, 2, 3, 6)]
    names(data.csv) <- c(t.label, label, "tide", "station")
    levels(data.csv$tide) <- c("H", "HH", "L", "LL")
  }
  
  if(interval == "6 minute" ) {
    data.csv$datetime <- as.POSIXlt(data.csv[, 1], format = "%Y-%m-%d %H:%M")
    data.csv <- data.csv[, c(10, 2, 9)]
    names(data.csv) <- c(t.label, label, "station")
  }
  
  if(interval == "hourly" ) {
    data.csv$datetime <- as.POSIXlt(data.csv[, 1], format = "%Y-%m-%d %H:%M")
    data.csv <- data.csv[, c(7, 2, 6)]
    names(data.csv) <- c(t.label, label, "station") 
  }
  
  if(interval == "monthly" ) {
    data.csv$datetime <- data.csv$Year + data.csv$Month / 12
  }
  
  
  if(continuous == "TRUE" | continuous == "T" & interval == "hourly") {
    data.csv <- data.csv[!duplicated(data.csv[, 1]), ]
    time.df <- data.frame(seq(from = data.csv[1, 1], to = data.csv[nrow(data.csv), 1], by = 60*60))
    names(time.df)[1] <- t.label
    data.csv <- join_all(list(time.df, data.csv[!duplicated(data.csv[, 1]), ]))
    
  } else if(continuous == "TRUE" | continuous == "T" & interval == "6 minute") {
    data.csv <- data.csv[!duplicated(data.csv[, 1]), ]
    time.df <- data.frame(seq(from = data.csv[1, 1], to = data.csv[nrow(data.csv), 1], by = 60*6))
    names(time.df)[1] <- t.label
    data.csv <- join_all(list(time.df, data.csv[!duplicated(data.csv[, 1]), ]))
    
  } else if(continuous == "TRUE" | continuous == "T" & interval == "HL") {
    print("'continuous' argument not used for high/low data")
    data.csv <- data.csv[!duplicated(data.csv[, 1]), ]
    
  } else if(continuous == "TRUE" | continuous == "T" & interval == "monthly") {
    time.df <- data.frame(seq(from = data.csv$datetime[1], to = data.csv$datetime[nrow(data.csv)], by = 1 / 12))
    names(time.df) <- "datetime"
    time.df$datetime <- round(time.df$datetime, 2)
    data.csv$datetime <- round(data.csv$datetime, 2)
    data.csv <- join_all(list(time.df, data.csv))
    data.csv$Year <- as.numeric(data.csv$Year)
    data.csv$Year[is.na(data.csv$station)] <- as.numeric(substr(data.csv$datetime[is.na(data.csv$station)], 1, 4))
    data.csv$Month[is.na(data.csv$station)] <- round((data.csv$datetime[is.na(data.csv$station)] - data.csv$Year[is.na(data.csv$station)]) * 12)
    data.csv$station[is.na(data.csv$station)] <- site.name
  } else data.csv <- data.csv[!duplicated(data.csv$datetime), ]
  
  invisible(data.csv)
  
    }
