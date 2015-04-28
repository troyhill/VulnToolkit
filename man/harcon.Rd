\name{harcon}
\alias{harcon}
\title{Scrapes harmonic constituent data from NOAA CO-OPS website}
\description{
  Scrapes harmonic constituent data from NOAA CO-OPS website. Requires internet connection.
}
\usage{
  harcon(station)
}
\arguments{
  \item{station}{station ID number, available on CO-OPS 
    website (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels) or 
    from \code{\link{noaa.stations}}.}
}
\value{
  \item{dataset}{a dataframe of harmonic constituents and their associated phases, amplitudes, and speeds.}
}
\examples{
# Example is commented out because it requires an internet connection
# bport.cons <- harcon(8467150) # Bridgeport, CT
# bport.cons}