### Function downloads and compiles tide data from NOAA CO-OPS website
### Requires RCurl and XML packages - install.packages("RCurl"); install.packages("XML")

### This version does not include a monthly time interval option
### Code updated 20131120

### Argument notes: 
### - "begindate" and "enddate" set desired date range and must be in YYYYMMDD format
###     if one or both dates are left unspecified, the first and/or last *full* day of 
###     available data will be used. This does not always work because of issues with the CO-OPS site.
### - "station" input is the station name or ID number, available on CO-OPS site (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels)
### - "units" can be feet or meters
### - "datum" can be station, NAVD, MLLW, MLW, MSL, MTL, MHW, MHHW, or IGLD (all datums are not available at all sites)
### - "interval" sets measurement interval; can be HL, 6 minute, or hourly
### - "time" can be LST, GMT, or LST/LDT. Not all time zones are available for 
###    all data. GMT appears to have wider availability than LST, so it is now default
###
### Have a problem? Find a bug? Email Hill.Troy@gmail.com


noaa <- function(begindate = "begindate", enddate = "enddate", station = "8467150",
                  units = "meters", datum = "MHW", interval = "HL", time = "GMT") {

#require(RCurl)
#require(XML)

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
     ti <- "6"   
      ti.csv <- "water_level"
     ti.name <- "Verified 6-Minute Water Level"
 } else if(interval ==  "hourly")             {
      ti <- "h" 
      ti.csv <- "hourly_height"
      ti.name <- "Verified Hourly Height Water Level"
 } else if(interval ==  "HL")                 {
      ti <- "hl"
      ti.csv <- "high_low"
      ti.name <- "Verified High/Low Water Level"
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
    } else if( as.numeric(edate - sdate) > 365) {
      dates <- seq(sdate,edate, 365)
    } else(dates <- c(sdate,edate))}
if(interval == "hourly") { if(sdate < first.rec | edate > last.rec) {
    stop("invalid time interval")
    } else if( as.numeric(edate - sdate) > 365) {
      dates <- seq(sdate,edate, 365)
    } else(dates <- c(sdate,edate))}
if(interval == "6 minute") { if(sdate < first.rec | edate > last.rec) {
    stop("invalid time interval")
    } else if( as.numeric(edate - sdate) > 31)  {
      dates <- seq(sdate,edate, 31)
    } else(dates <- c(sdate,edate))}

if(!edate %in% dates[length(dates)]) dates <- c(dates, edate)

dates2 <- format(as.Date(dates), "%Y%m%d")     # re-format dates for the url


# create list of csv files
for(i in 1:(length(dates2) - 1)) {
  url.temp <- c(paste("http://co-ops.nos.noaa.gov/api/datagetter?product=", ti.csv, 
  "&application=NOS.COOPS.TAC.WL&station=", station,  "&begin_date=", dates2[i], 
  "&end_date=", dates2[i+1], "&datum=", datum, "&units=", u.csv, "&time_zone=", tz, 
  "&format=csv", sep=""))
  if (!exists("url.list")){
      url.list <- url.temp
      }
  # if the dataset exists, add to it
  if (exists("url.list")){
     url.list[i] <- url.temp
     rm(url.temp)
        }
  }


lapply.csv <- lapply(url.list, function(x)read.csv(x))
data.csv <- do.call(rbind, lapply.csv)
data.csv$station <- rep(site.name, times = nrow(data.csv))

label <- paste("verified water level at ", site.name, " (", units, " rel. to ", datum, ")", sep="")
t.label <- paste("time (", time, ")", sep = "")


# clean up the data
if(interval == "HL" ) {
  data.csv$datetime <- as.POSIXct(data.csv[,1], format = "%Y-%m-%d %H:%M")
  data.csv <- data.csv[, c(7,2,3,6)]  
  names(data.csv) <- c(t.label, label, "tide", "station")
  levels(data.csv$tide) <- c("H", "HH", "L", "LL")
  }
                      
if(interval == "6 minute" ) {
  data.csv$datetime <- as.POSIXct(data.csv$Date.Time, format = "%Y-%m-%d %H:%M")
  data.csv <- data.csv[, c(10,2,9)] 
  names(data.csv) <- c(t.label, label, "station") 
  }

if(interval == "hourly" ) {
  data.csv$datetime <- as.POSIXct(data.csv$Date.Time, format = "%Y-%m-%d %H:%M")
  data.csv <- data.csv[, c(7,2,6)]
  names(data.csv) <- c(t.label, label, "station") 
  }

           

invisible(data.csv[!duplicated(data.csv[,1]),])

}
