\name{wave.dur}
\alias{wave.dur}
\title{find the proportion of time when water surface is near a selected elevation}
\description{
  calculates the proportion of time when water surface is within a vertical range (defined by \code{buffer}) of \code{elevation}. This is designed to estimate low-level wave exposure. 
}
\usage{
wave.dur(level, elevation, buffer = 0.050)
}
\arguments{
  \item{level}{a numeric vector of water levels, in meters if \code{buffer} is left as default}

  \item{elevation}{elevation(s) of interest (e.g., marsh platform, MHW). A vector of elevations is accepted. Elevation should be in the same vertical datum as \code{level} and have units of meters}

  \item{buffer}{the vertical range used to calculate exposure. Default is 0.050 meters (5 cm)}
}
\value{
  \item{output}{a vector of values representing the fraction of time in the entire dataset where the water surface is near the elevation of interest. Numerical range is from 0-1}

}
\examples{
data(NL_6min_2013)
MHW <- 0.9117  # New London MHW in 2013: 0.9117 m relative to MLLW

wave.dur(NL_6min_2013[, 2], MHW)

# function accepts and returns vectors
elevs           <- data.frame(elevs = c(-1000:1500) / 10^3)
elevs$wave.dur  <- wave.dur(NL_6min_2013[, 2], elevs$elevs)
plot(elevs)
}
