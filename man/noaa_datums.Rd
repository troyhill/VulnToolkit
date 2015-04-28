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
<<<<<<< HEAD
  \item{station}{station name or ID number, available on CO-OPS 
    website (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels). 
    Default is Bridgeport, CT station.}
=======
  \item{station}{station name or ID number, available on CO-OPS site (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels). Default is Bridgeport, CT station.}
>>>>>>> d0e32ede8235fca8b8a3e150d38eeaada1b1f302
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
<<<<<<< HEAD
# battery.datums <- noaa.datums(station = 8518750) # Battery, NYC}
=======
# battery.datums <- noaa.datums(station = 8518750) # Battery, NYC
}
>>>>>>> d0e32ede8235fca8b8a3e150d38eeaada1b1f302
