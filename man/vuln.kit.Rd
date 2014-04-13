\name{vuln.kit}
\alias{vuln.kit}
\title{Calculates selected hydrologic parameters and vulnerability metrics}
\description{
  This function takes a set of water level data as an input, and calculates a set of flooding parameters.
}
\usage{
vuln.kit(level, datetime, platform, units = "meters", frq.dur.inc = 0.005, 
                     TV.inc = 0.1)
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
\item{output}{a list containing two items:}

\item{dataset}{a dataframe of elevations (relative to elevation set in \code{platform} argument), flooding frequencies (flooding events per year), flooding durations (hr yr-1), duration of 90th percentile flooding event (D90; hr), and mean flooding depth (A; m)}

\item{metrics}{a dataframe containing the flooding frequency, flooding duration, D90, mean flooding depth, duration vulnerability, and D90 vulnerability, calculated at the vertical elevation set by \code{platform} argument}

}
\examples{
data(NL_6min_2013)
NL2013 <- vuln.kit(level = NL_6min_2013[,2], datetime = NL_6min_2013[,1],
                  platform = 0.9117) # MHW in 2013: 0.9117 m relative to MLLW
NL2013$metrics

}
