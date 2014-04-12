\name{noaa}
\alias{noaa}
\title{Scrapes water level data from NOAA-COOPS website}
\description{
  This is the description string for noaa().
}
\usage{
  
}
\arguments{
  \item{level}{a numeric vector of water levels}
  
  \item{datetime}{a POSIX* vector of time stamps that correspond to the measurements in 'level'}
  
  \item{platform}{elevation of the marsh platform (or another vertical position of interest). Should be in the units specified by \code{units} and relative to the same vertical datum as \code{level}.}
  
  \item{units}{length units used. 'meters' is default; 'feet' is alternative. If units are "feet," data is converted internally and output in meters.}
  
  \item{frq.dur.inc}{elevation interval used to calculate flooding frequency, duration, D90, and Ax. Defaults to 0.005 m. Units must correspond to \code{units} argument}
  
  \item{TV.inc}{Elevation interval used to calculate vulnerability metrics (DV, D90V). Defaults to 0.1 m. Units must correspond to \code{units} argument.}
  
}
\value{
  \item{dataset}{a dataset of elevations (relative to elevation set in \code{platform} argument), flooding frequencies, flooding durations (hr yr-1), duration of 90th percentile flooding event (D90; hr), and mean flooding depth (m)}
}