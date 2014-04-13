\name{noaa_datums}
\alias{noaa_datums}
\title{Scrapes elevation datums from NOAA-COOPS website}
\description{
  Scrapes elevation datums from NOAA-COOPS website. Requires internet connection.
}
\usage{
  noaa_datums(station)
}
\arguments{
  \item{station}{station name or ID number, available on CO-OPS site (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels). Default is Bridgeport, CT station.}
}
\value{
  \item{dataset}{a dataframe of vertical datum names and their elevations in meters relative to the station datum for the 1983-2001 epoch. Also contains a column of times associated with relevant datums (record maximum and minimums, lowest and highest astronomical tides).}
}
\examples{
# Example is commented out because it requires an internet connection
# bport.datums <- noaa_datums(station = 8467150) # Bridgeport, CT
}