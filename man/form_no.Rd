\name{form.no}
\alias{form.no}
\title{Calculate tidal form number}
\description{
  Uses harmonic constituent data from the NOAA CO-OPS website to calculate tidal form numbers as the ratio of the sum of K1 and O1 diurnal harmonic constituent amplitudes to the sum of the M2 and S2 semidiurnal amplitudes. Requires internet connection.
}
\usage{
  form.no(station)
}
\arguments{
  \item{station}{station ID number(s), available on CO-OPS site (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels). A list}
}
\value{
  \item{dataset}{a dataframe of station number(s) and corresponding tidal form number(s).}
}
\examples{
# Example is commented out because it requires an internet connection
# a <- form.no()
# stn.list <- c("8467150", "8461490", "9454240")
# b <- form.no(stn.list)
# b
}