\name{noaa.datums}
\alias{noaa.datums}
\title{Scrapes elevation datums from NOAA CO-OPS website}
\description{
  Scrapes elevation datums from NOAA CO-OPS website. Requires internet connection.
}
\usage{
  noaa.datums(station = 8467150)
}
\arguments{
  \item{station}{station name or ID number, available on CO-OPS 
    website (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels). 
    Default is Bridgeport, CT station.}
}
\value{
  \item{dataset}{a dataframe of vertical datum names and their elevations in meters relative to the station datum for the 1983-2001 epoch. Also contains a column of times associated with relevant datums (record maximum and minimums, lowest and highest astronomical tides).}
}
\references{
http://wetlandsandr.wordpress.com/
}
\examples{
# Example is commented out because it requires an internet connection
# bport.datums <- noaa.datums() # Bridgeport, CT
# battery.datums <- noaa.datums(station = 8518750) # Battery, NYC}