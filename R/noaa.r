#' @title Downloads NOAA CO-OPS tide data
#'
#' @description
#' Scrapes water level data (and other measurements) from NOAA CO-OPS website. Requires 
#' internet connection.
#'
#' @details 
#' Download water level and other data from NOAA CO-OPS website.
#' 
#' @usage noaa(begindate = "begindate", enddate = "enddate", station = "8467150",
#' met = "FALSE", units = "meters", datum = "MHW", interval = "HL", time = "GMT", 
#' continuous = "TRUE")
#' 
#' @param begindate first day of data to download. Format must be YYYYMMDD. If 
#' left unspecified, the first complete day of data will be used.
#' 
#' @param enddate final day of data to download. Format must be YYYYMMDD. If left 
#' unspecified, the last complete day of data will be used.
#' 
#' @param station station name or ID number, available on the CO-OPS website or by 
#' using \code{\link{noaa.stations}}. Entry can be numeric (station ID) or a string 
#' corresponding to the station name. Default station is Bridgeport, CT.
#' 
#' @param met whether meteorological data should be returned. This value can be 'TRUE' or 
#' 'FALSE'; if 'TRUE', all ancillary parameters are returned. At present, this only 
#' works with 6-minute and hourly data
#' 
#' @param units can be 'feet' or 'meters'. Default is 'meters'
#' 
#' @param datum vertical reference datum, set to 'MHW' by default. Can be 'station', 'NAVD', 
#' 'MLLW', 'MLW', 'MSL', 'MTL', 'MHW', 'MHHW', or 'IGLD' (some datums are not available 
#' at some sites)
#' 
#' @param interval sets measurement interval; can be 'HL' (default), '6 minute', 'hourly', or 'monthly'. 
#' For data on monthly and annual time scales from Permanent Service for Mean Sea Level, 
#' see \code{\link{psmsl}}
#' 
#' @param time can be 'LST', 'GMT', or 'LST/LDT'. Not all time zones are available for all data. 
#' GMT appears to have wider availability than LST, so it is the default.
#' 
#' @param continuous determines whether a continuous time series is produced, with lengthy gaps 
#' in data filled in with NAs. By default, this is \code{FALSE}. This option only applies 
#' to data at evenly spaced intervals (i.e., \code{6 minute} or \code{hourly})
#' 
#' @return dataset a dataframe with water levels, associated time stamps, a station ID column, 
#' and tide type (if interval is set to \code{HL}). The NOAA CO-OPS website has many odd data 
#' availabilty problems. Some data are not available in all time intervals or time zones.
#' 
#' @seealso \code{\link{noaa.stations}}
#' 
#' @import XML
#' @import plyr
#' @importFrom stats complete.cases
#' @importFrom utils read.csv
#' 
#' 
#' @examples \dontrun{
# Example requires an internet connection
#'bport2013 <- noaa(begindate = 20130101, enddate = 20131231, 
#'   station = "Bridgeport, CT", interval = "6 minute")
#'
#'test2.1 <- noaa("20100101", "20120101", interval = "hourly") 
#'test2.2 <- noaa("20100101", "20120101", interval = "hourly", continuous = "TRUE") 
#'nrow(test2.1) # includes data on NOAA site (incomplete record)
#'nrow(test2.2) # fills gaps with NAs 
#' }
#' @export



noaa <- function(begindate = "begindate", enddate = "enddate", station = "8467150",
                 met = "FALSE", units = "meters", datum = "MHW", interval = "HL", 
                 time = "GMT", continuous = "TRUE") {
  getDates <- function(startDate, endDate, dataType, 
                       first.record = startDate, last.record = endDate) {
    # function produces a vector of dates used to download data
    startDate <- as.Date(gsub("-", "", startDate), tz = posix.tz, format = "%Y%m%d")
    endDate   <- as.Date(gsub("-", "", endDate), tz = posix.tz, format = "%Y%m%d")
    
    dates <- startDate
    if (dataType == "HL") { 
      if (startDate < first.record | endDate > last.record)   {
        stop ("invalid time interval")
      } else if (as.numeric(endDate - startDate) > 364) {
        dates <- seq(startDate, endDate, 365)
      } else (dates <- c(startDate, endDate))
    }
    if (dataType == "6 minute") {
      if (startDate < first.record | endDate > last.record)   {
        stop ("invalid time interval")
      } else if (as.numeric(endDate - startDate) > 30)  {
        dates <- seq(startDate, endDate, 31)
      } else (dates <- c(startDate, endDate))
    }
    if (dataType == "hourly") {
      if (startDate < first.record | endDate > last.record)   {
        stop ("invalid time interval")
      } else if ( as.numeric(endDate - startDate) > 364) {
        dates <- seq(startDate, endDate, 365)
      } else (dates <- c(startDate, endDate))
    }
    
    if (!endDate %in% dates[length(dates)]) {
      dates <- c(dates, endDate)
    }
    
    returnedDates <- gsub("-", "", as.character(dates))     # re-format dates for the url
    returnedDates <- returnedDates[!is.na(returnedDates)]
  }
  
  
  # set acceptable true/flase values
  T.vals  <- c("TRUE", "T", "True", TRUE, T)
  F.vals  <- c("FALSE", "F", "False", FALSE, F)
  TF.vals <- c(T.vals, F.vals)
  
  if (!continuous %in% TF.vals) 
    stop ("'continuous' must be set to 'TRUE' or 'FALSE'")
  
  if ((interval %in% c("HL", "monthly")) & (!met %in% F.vals)) {
    met <- "FALSE"
    print("`met = TRUE` is not consistent with monthly or HL water levels. If meteorological data are desired, request 6 minute or hourly data.")
  }
  
  # set units                                                       
  if (units ==  "meters")       {
    u.csv <- u <- "metric"
  } else if (units ==  "feet")  {
    u     <- "standard"
    u.csv <- "english"
  } else stop ("invalid units: must be 'feet' or 'meters' ")
  
  # set datum
  if (datum %in% c("STND", "MHHW", "MHW", "MTL", "MSL", "MLW", "MLLW", "NAVD", "IGLD")) {
    datum <- datum  
  } else stop ("invalid datum: must be 'STND', 'MHHW', 'MHW', 'MTL',
    'MSL', 'MLW', 'MLLW', 'IGLD', or 'NAVD'")
  
  # set measurement time interval        
  if (interval ==  "6 minute")                  {
    ti.csv  <- "water_level"
    ti.name <- "Verified 6-Minute Water Level"
    met.csv <- "6" # this variable is for calling meteorological data csv files
  } else if (interval ==  "hourly")             {
    ti.csv  <- "hourly_height"
    ti.name <- "Verified Hourly Height Water Level"
    met.csv <- "h"
  } else if (interval ==  "HL")                 {
    ti.csv  <- "high_low"
    ti.name <- "Verified High/Low Water Level"
  } else if (interval ==  "monthly")            {
    ti.csv  <- "monthly_mean"
    ti.name <- "Verified Monthly Mean Water Level"
  }  else stop ("invalid time interval: must be '6 minute', 'hourly', or 'HL'")
  
  # set time zone
  if (time %in%  c("LST/LDT", "GMT", "LST"))    {
    tz <- time
  } else stop ("invalid time zone: must be 'LST/LDT', 'GMT', or 'LST' ")
  
  # set time zone in more detail (for labeling data in conversion to POSIX)
  # LST/LDT is not ideal and, at present, probably produces mislabelled data
  if (time %in%  c("GMT"))    {
    posix.tz <- time
  } else if (time %in%  c("LST/LDT", "LST"))    {
    posix.tz <- ""  # if not specified, the local time zone used (relative to computer, 
    # not the data!). I don't think this will alternate between LST and LDT.
  } 
  
  # set site name/number indicator
  if (regexpr("[0-9]{7}", station)[1] == 1)         {
    site.ind  <- c(1)
  } else if (regexpr("[a-zA-Z]+", station)[1] == 1) {
    site.name <- station
    site.ind  <- c(0)
  } else stop("Invalid station entry: must use station name or number. Check active stations 
   at: https://www.tidesandcurrents.noaa.gov/stations.html?type=Water+Levels")
  
  # nocov start
  
  
  stns <- readLines("https://www.tidesandcurrents.noaa.gov/stations.html", warn = FALSE) # list of active stations
  
  if (site.ind == 1) {                                                            # Use station number to identify station
    stn1 <- grep(paste(station, " " , sep = ""), stns)                            # station number is followed by a space, then the station name
    if (length(stn1) == 0)        {
      stop ("Station number appears to be invalid. No match found at
            https://www.tidesandcurrents.noaa.gov/stations.html?type=Water+Levels")
    } else if (length(stn1) > 1)  {
      stop ("Station number appears to be duplicated. Try using site name:
            https://www.tidesandcurrents.noaa.gov/stations.html?type=Water+Levels")
    } else if (length(stn1) == 1) {
      stn2      <- regexpr("[0-9] .*</a>$", stns[stn1])
      stn3      <- regmatches(stns[stn1],   stn2)                                 # extract matches
      site.name <- gsub("[0-9] |</a>", "",  stn3)                                 # clean up site name
    }
  } else if (site.ind == 0) {                                                     # Use station name to identify site number
    no1     <- grep(site.name, stns)                                                    
    if (length(no1) == 1) { 
      no2     <- regexpr("[0-9]{7} .*</a>$", stns[no1])
      no3     <- regmatches(stns[no1], no2)                                       
      station <- site.no <- gsub("[A-Za-z]| |,|</a>", "", no3)                           
    } else if (length(no1) > 1) {
      stop ("Site name found for multiple active NOAA stations. Look up site number at 
            https://www.tidesandcurrents.noaa.gov/stations.html?type=Water+Levels")
    } else if (length(no1) < 1) {
      stop ("Site name not found on list of active NOAA stations. Look up sites at 
            https://www.tidesandcurrents.noaa.gov/stations.html?type=Water+Levels. 
            Be attentive to spelling or consider using the station number.")
    }
    }
  
  
  siteParameters <- noaa.parameters(stn = station)
  
  ### find line with data product
  # if there are multiple rows per product, use start date from first line 
  data.line    <- grep(ti.name, siteParameters$params)[1] 
  first.record <- siteParameters$startDate[data.line]
  first.rec    <- as.Date(first.record, format = "%Y%m%d")
  
  data.line.end    <- grep(ti.name, siteParameters$params)[length(grep(ti.name, siteParameters$params))] 
  last.record <- siteParameters$endDate[data.line.end]
  last.rec    <- as.Date(last.record, format = "%Y%m%d")
  ### This should be more robust to data gaps
  
  # set start/end dates to full period of record, if left as default
  if (begindate ==  "begindate")        {
    sdate <- first.rec + 1
  } else if (begindate !=  "begindate") {
    sdate <- as.Date(as.character(begindate), "%Y%m%d")
  }
  
  if (enddate ==  "enddate")            {
    edate <- last.rec - 1
  } else if (enddate !=  "enddate")     {
    edate <- as.Date(as.character(enddate), "%Y%m%d")
  }
  
  
  
  # check if date range is within period of record, and check if time period
  # requires splitting into smaller units. Interval limit is 1 year for hourly
  # and HL data,  31 days for 6-min data, 10 years for monthly data.
  
  
  dates2 <- getDates(startDate = sdate, endDate = edate, dataType = interval, 
                     first.record = first.rec, last.record = last.rec)
  
  #####
  ##### Get water level data
  #####
  # create list of csv files for water level data
  for(i in 1:(length(dates2) - 1)) {
    url.temp <- c(paste0("https://tidesandcurrents.noaa.gov/api/datagetter?", 
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
  
  
  # RCurl dependency eliminated 20200903
  oldTimeout <- options()$timeout
  options(timeout = 20) # set readLines timeout to 20 secs
  lapply.csv <- lapply(url.list, function(x) readLines(x))
  
  for (i in 1:length(lapply.csv)) {
    txtCSV <- textConnection(lapply.csv[[i]])
    
    if (!exists("data.csv")){
      data.csv <- utils::read.csv(txtCSV)
    } else if (exists("data.csv")){
      data.csv <- rbind(data.csv, utils::read.csv(txtCSV))
      rm(txtCSV)
    }
  }
  
  data.csv$station <- rep(site.name, times = nrow(data.csv))
  
  label <- paste("verified water level at ", site.name, " (", units, " rel. to ", datum, ")", sep="")
  t.label <- paste("time (", time, ")", sep = "")
  
  
  # clean up the data
  if(interval == "HL" ) {
    data.csv$datetime <- as.POSIXct(data.csv[, 1], format = "%Y-%m-%d %H:%M", tz = posix.tz)
    data.csv <- data.csv[, c(7, 2, 3, 6)]
    names(data.csv) <- c(t.label, label, "tide", "station")
    levels(data.csv$tide) <- c("H", "HH", "L", "LL")
  }
  
  if(interval == "6 minute" ) {
    data.csv$datetime <- as.POSIXct(data.csv[, 1], format = "%Y-%m-%d %H:%M", tz = posix.tz)
    data.csv <- data.csv[, c(10, 2, 9)]
    names(data.csv) <- c(t.label, label, "station")
  }
  
  if(interval == "hourly" ) {
    data.csv$datetime <- as.POSIXct(data.csv[, 1], format = "%Y-%m-%d %H:%M", tz = posix.tz)
    data.csv <- data.csv[, c(7, 2, 6)]
    names(data.csv) <- c(t.label, label, "station") 
  }
  
  if(interval == "monthly" ) {
    data.csv$datetime <- data.csv$Year + data.csv$Month / 12
  }
  
  
  if(interval == "hourly" & (continuous == "TRUE" | continuous == "T")) {
    data.csv <- data.csv[!duplicated(data.csv[, 1]), ]
    time.df <- data.frame(seq(from = data.csv[1, 1], to = data.csv[nrow(data.csv), 1], by = 60*60))
    names(time.df)[1] <- t.label
    data.csv <- plyr::join_all(list(time.df, data.csv[!duplicated(data.csv[, 1]), ]))
    
  } else if(interval == "6 minute" & (continuous == "TRUE" | continuous == "T")) {
    data.csv <- data.csv[!duplicated(data.csv[, 1]), ]
    time.df <- data.frame(seq(from = data.csv[1, 1], to = data.csv[nrow(data.csv), 1], by = 60*6))
    names(time.df)[1] <- t.label
    data.csv <- plyr::join_all(list(time.df, data.csv[!duplicated(data.csv[, 1]), ]))
    
  } else if(interval == "monthly" & (continuous == "TRUE" | continuous == "T" )) {
    time.df <- data.frame(seq(from = data.csv$datetime[1], to = data.csv$datetime[nrow(data.csv)], by = 1 / 12))
    names(time.df) <- "datetime"
    time.df$datetime <- round(time.df$datetime, 2)
    data.csv$datetime <- round(data.csv$datetime, 2)
    data.csv <- plyr::join_all(list(time.df, data.csv))
    data.csv$Year <- as.numeric(data.csv$Year)
    data.csv$Year[is.na(data.csv$station)] <- as.numeric(substr(data.csv$datetime[is.na(data.csv$station)], 1, 4))
    data.csv$Month[is.na(data.csv$station)] <- round((data.csv$datetime[is.na(data.csv$station)] - data.csv$Year[is.na(data.csv$station)]) * 12)
    data.csv$station[is.na(data.csv$station)] <- site.name
  } else data.csv <- data.csv[!duplicated(data.csv[, 1]), ]
  
  
  #####
  ##### Get meteorological data (if desired)
  #####
  # create list of csv files for met data
  # I may need to verify that data is available for requested range, 
  # or ignore csv files that return blank data
  
  # 150809: If no met data are available during the time period specified, an error is thrown. 
  # This shouldn't happen.
  
  if (!met %in% F.vals) {
    
    # get available products for the station, and corresponding dates
    param_list   <- data.frame(names = c("Conductivity", "Wind", "Barometric Pressure", "Air Temperature",
                                         "Water Temperature", "Relative Humidity", "Salinity"), 
                               codes = c("conductivity", "wind", "air_pressure", "air_temperature", "water_temperature",
                                         "humidity", "salinity")
    )
    
    #   siteParameters
    #   date.list    <- lapply(TempNodes, function(x)  xmlValue(getSibling(x))) # first entry is consistently skipped
    #   availableParams    <- data.frame(param = NA, start = NA, end = NA)
    availableParams    <- siteParameters[siteParameters$params %in% param_list$names, ]
    
    # check that 'met' is valid.
    # "TRUE" causes met to download all available data
    if (sum(met %in% availableParams$params) > 0) {
      availableParams <- availableParams[availableParams$params %in% met, ] # select only those sought params that are also available
    } else if (sum(met %in% availableParams$params) == 0) {
      message("`met` parameters were not recognized. All meteorological parameters will be downloaded.")
    }
    
    # add if clause: if availableParams has more than zero rows, run this
    if (!nrow(availableParams) == 0) {
      availableParams$actualEnd <- availableParams$actualStart <- as.numeric(NA) # start and end dates to use when calling data
      
      for (i in 1:nrow(availableParams)) {
        #       # if a param is found, check if date range is relevant
        if (availableParams$startDate[i] < enddate) { # if data starts before request ends, fill in data
          # if the date range is relevant, record the details
          # first check whether the date range is relevant, then if it is include the parameter 
          # old version converted to times before comparing: if (as.integer(as.POSIXct(as.character(availableParams$startDate[i]), format = "%Y%m%d", origin = posix.tz)) <= as.integer(as.POSIXct(as.character(enddate), format = "%Y%m%d", origin = posix.tz))) {
          if (availableParams$startDate[i] <= enddate) {
            temp.Params <- data.frame(param = as.character(param_list$codes[i]), start = NA, end = NA)
            
            if ((availableParams$startDate[i] <= begindate) & !(availableParams$endDate[i] < begindate)) {
              availableParams$actualStart[i]    <- begindate
            } else if ((availableParams$startDate[i] <= begindate) & (availableParams$endDate[i] < begindate) ) {
              availableParams$actualStart[i]    <- as.numeric(NA)
            } else if (availableParams$startDate[i] > begindate) {
              availableParams$actualStart[i]    <- as.numeric(availableParams$startDate[i])
            }
            
            if (availableParams$endDate[i] >= enddate) {
              availableParams$actualEnd[i]      <- enddate
            } else if (availableParams$endDate[i] < enddate) {
              availableParams$actualEnd[i]      <- as.numeric(availableParams$endDate[i])
            }
            if (is.na(availableParams$actualStart[i])) {
              availableParams$actualEnd[i]    <- as.numeric(NA)
            }
          }
        }
      }
      
      ### parameters relevant to our time period of interest
      availableParams <- availableParams[stats::complete.cases(availableParams), ]
      # availableParams has each available ancillary parameter, and their associated start and end dates
      
      
      for (i in 1:nrow(availableParams)) {
        dateRange <- getDates(startDate = availableParams$actualStart[i], endDate = availableParams$actualEnd[i], dataType = interval)
        
        for(j in 1:(length(dateRange) - 1)) {
          met.url.temp <- c(paste0("https://tidesandcurrents.noaa.gov/api/datagetter?", 
                                   "product=", param_list$codes[match(availableParams$param[i], param_list$names)],
                                   "&application=NOS.COOPS.TAC.PHYSOCEAN",
                                   "&begin_date=", dateRange[j], 
                                   "&end_date=", dateRange[j+1], 
                                   "&station=", station,
                                   "&time_zone=", tz, 
                                   "&units=", u.csv,
                                   "&interval=", met.csv,
                                   "&format=csv"))
          
          
          if (!exists("met.url.list")) {
            met.url.list    <- met.url.temp
          }
          # if the dataset exists, add to it
          if (exists("met.url.list")) {
            met.url.list[j] <- met.url.temp
            rm(met.url.temp)
          }
        }
        
        # RCurl dependency removed 20200903
        met.lapply.csv   <- lapply(met.url.list, function(x) readLines(x))
        
        for (k in 1:length(met.lapply.csv)) {
          if (!exists("met.data.csv")) {
            met.data.csv <- utils::read.csv(textConnection(met.lapply.csv[[k]]))
          }
          # if the dataset exists, add to it
          if (exists("met.data.csv")) {
            txtCSV       <- textConnection(met.lapply.csv[[k]])
            met.data.csv <- rbind(met.data.csv, utils::read.csv(txtCSV))
            rm(txtCSV)
          } 
        } 
        
        # now, all data is compiled for availableParams$param[i] 
        # so, merge it in.
        
        # but first, remove bloated columns
        rem           <- match(c("X", "N", "R"), names(met.data.csv))
        met.data.csv  <- met.data.csv[, -c(rem[!is.na(rem)])]
        
        # get datetime variable
        # 1. convert to character
        met.data.csv[, 1]    <- as.character(met.data.csv[, 1])
        # 2. re-structure to remove backslashes
        t.temp <- gsub("-", "", met.data.csv[, 1])
        
        # join using seconds since 1970
        met.data.csv$datetime <- as.integer(as.POSIXct(t.temp, format = "%Y%m%d %H:%M", tz = posix.tz))
        data.csv$datetime     <- as.integer(data.csv[, 1])
        
        data.csv              <- plyr::join_all(list(data.csv, met.data.csv[, -1]), by = "datetime")
        
        # now, remove datetime integer column
        datetime.col          <- grep("datetime", names(data.csv))
        data.csv              <- data.csv[, -c(datetime.col)] 
        invisible(data.csv)
        
        rm(met.url.list)
        rm(met.data.csv)
      }
    } # closes section contingent on availableParams haveing >0 rows
    invisible(data.csv)
  }
  options(timeout = oldTimeout)
  
  invisible(data.csv)
  } # nocov end
