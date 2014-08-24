\name{noaa}
\alias{noaa}
\title{Downloads NOAA CO-OPS tide data}
\description{
  Scrapes water level data from NOAA CO-OPS website. Requires internet connection.
}
\usage{
  noaa(begindate = "begindate", enddate = "enddate", station = "8467150",
                  units = "meters", datum = "MHW", interval = "HL", time = "GMT", continuous = "FALSE")
}
\arguments{
  \item{begindate}{first day of data to download. Format must be YYYYMMDD. If left unspecified, the first complete day of data will be used.}
  \item{enddate}{final day of data to download. Format must be YYYYMMDD. If left unspecified, the last complete day of data will be used.}
  \item{station}{station name or ID number, available on the CO-OPS website or by using \code{\link{noaa.stations}}. Entry can be numeric (station ID) or a string corresponding to the station name. Default station is Bridgeport, CT.}
  \item{units}{can be 'feet' or 'meters'. Default is 'meters'}
  \item{datum}{vertical reference datum, set to 'MHW' by default. Can be 'station', 'NAVD', 'MLLW', 'MLW', 'MSL', 'MTL', 'MHW', 'MHHW', or 'IGLD' (some datums are not available at some sites)}
  \item{interval}{interval sets measurement interval; can be 'HL' (default), '6 minute', 'hourly', or 'monthly'. For data on monthly and annual time scales from Permanent Service for Mean Sea Level, see \code{\link{psmsl}}}
  \item{time}{can be 'LST', 'GMT', or 'LST/LDT'. Not all time zones are available for all data. GMT appears to have wider availability than LST, so it is the default.}
  \item{continuous}{determines whether a continuous time series is produced, with lengthy gaps in data filled in with NAs. By default, this is \code{FALSE}. This option only applies to data at evenly spaced intervals (i.e., \code{6 minute} or \code{hourly})}
}
\value{
  \item{dataset}{a dataframe with water levels, associated time stamps, a station ID column, and tide type (if interval is set to \code{HL})}
}
\section{Warning }{
The NOAA CO-OPS website has many odd data availabilty problems. Some data are not available in all time intervals or time zones. For example:
# 
}
\seealso{
\code{\link{noaa.stations}}
}
\examples{
# Example requires an internet connection
# bport2013 <- noaa(begindate = 20130101, enddate = 20131231, 
#     station = "Bridgeport, CT", interval = "6 minute")
#
# test2.1 <- noaa("20100101", "20120101", interval = "hourly") 
# test2.2 <- noaa("20100101", "20120101", interval = "hourly", continuous = "TRUE") 
# nrow(test2.1) # includes data on NOAA site (incomplete record)
# nrow(test2.2) # fills gaps with NAs
}