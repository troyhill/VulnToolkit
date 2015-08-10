\name{noaa.parameters}
\alias{noaa.parameters}
\title{Reports data available for a NOAA station}
\description{
  noaa.parameters reports the parameters (meteorological and tidal) available for a specified NOAA station.
}
\usage{
  noaa.parameters(stn = 8518750)
}
\arguments{
  \item{stn}{NOAA station number (note that station names are not yet acceptable inputs).}
}
\value{
  \item{dataset}{a dataframe with parameters and associated start and end dates. Where a parameter's availability is not continuous, multiple rows are reported. This function can be used to, for example, select meteorological parameters to include in calls to \code{\link{noaa}}}
}
\seealso{
\code{\link{noaa.stations}}
\code{\link{noaa}}
}
\examples{
# Example requires an internet connection
# noaa.parameters()
# LA.stns <- noaa.stations(state = "LA")
# noaa.parameters(LA.stns$number[1])
}