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
<<<<<<< HEAD
  \item{station}{station ID number, available on CO-OPS 
    website (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels) or 
    from \code{\link{noaa.stations}}.}
=======
  \item{station}{station ID number, available on CO-OPS site (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels) or from \code{\link{noaa.stations}}.}
>>>>>>> d0e32ede8235fca8b8a3e150d38eeaada1b1f302
}
\value{
  \item{dataset}{a dataframe of harmonic constituents and their associated phases, amplitudes, and speeds.}
}
\examples{
# Example is commented out because it requires an internet connection
# bport.cons <- harcon(8467150) # Bridgeport, CT
<<<<<<< HEAD
# bport.cons}
=======
# bport.cons
}
>>>>>>> d0e32ede8235fca8b8a3e150d38eeaada1b1f302
