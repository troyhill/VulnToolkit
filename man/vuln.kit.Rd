\name{vuln.kit}
\alias{vuln.kit}
\title{Calculates selected hydrologic parameters and vulnerability metrics}
\description{
  This is the description string for vuln.kit.
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

  \item{eps}{is a small positive value.  The program currently does not
handle exact observed death times t, but replaces these by interval
censored observations (t-eps,t).}

  \item{adtim}{is a vector of additional time points at which the
survival function is to be computed.  If this is not specified, then the
survival function is only computed at the endpoints of the data
intervals.}


}