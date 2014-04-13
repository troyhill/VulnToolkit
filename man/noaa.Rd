\name{noaa}
\alias{noaa}
\title{Scrapes water level data from NOAA-COOPS website}
\description{
  Scrapes water level data from NOAA-COOPS website. Requires internet connection.
}
\usage{
  noaa(begindate = "begindate", enddate = "enddate", station = "8467150",
                  units = "meters", datum = "MHW", interval = "HL", time = "GMT")
}
\arguments{
  \item{begindate, enddate}{set desired date range and must be in YYYYMMDD format. If one or both dates are left unspecified, the first and/or last complete day of available data will be used.}
  
  \item{station}{station name or ID number, available on CO-OPS site (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels). Default is Bridgeport, CT station.}
  
  \item{units}{can be 'feet' or 'meters'. Default is 'meters'}
  
  \item{datum}{vertical reference datum, set to 'MHW' by default. Can be 'station', 'NAVD', 'MLLW', 'MLW', 'MSL', 'MTL', 'MHW', 'MHHW', or 'IGLD' (some datums are not available at some sites)}
  
  \item{interval}{sets measurement interval; can be 'HL' (default), '6 minute', or 'hourly'}
  
  \item{time}{can be 'LST', 'GMT', or 'LST/LDT'. Not all time zones are available for all data. GMT appears to have wider availability than LST, so it is the default.}
  
}
\value{
  \item{dataset}{a dataframe of water levels, associated time stamps, a station ID column, and tide type (if interval is set to 'HL')}
}
\examples{
# Example is commented out because it requires an internet connection
# bport2013 <- noaa(begindate = 20130101, enddate = 20131231, 
#     station = "Bridgeport, CT", interval = "6 minute")
}