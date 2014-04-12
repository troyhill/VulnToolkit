\name{HL}
\alias{HL}
\title{Extracts high and low tides from a record of water levels}
\description{
  Extracts high and low tides from a record of water levels
}
\usage{
  HL(level, time, period = 13, phantom = TRUE, tides = "all")
}
\arguments{
  \item{level}{a numeric vector of water levels}

  \item{time}{a vector (numeric or POSIX*) indicating the time of water level measurements. Units must be minutes.}

  \item{period}{a single numeric or integer estimate of tidal period (full tidal cycle). Units must be hours.}

  \item{phantom}{a protective measure taken to prevent the inclusion of an artificial high or low tide at the end of the dataset. If the water level measurements end precisely at a low or high tide, this can be changed to FALSE.}

  \item{tides}{is used to optionally subset the output to include only high or low tides. This argument can be 'all' (default), 'H', or 'L'}

}
\value{
  \item{dataset}{a dataframe of tide levels, associated time stamps, and tide type ('H' or 'L').}
}