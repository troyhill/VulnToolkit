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
  \item{dataset}{a dataframe of vertical datum names and their elevations in meters relative to the station datum. Also contains a column of times associated with relevant datums (record maximum and minimums, lowest and highest astronomical tides).}
}