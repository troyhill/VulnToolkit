\name{HL.plot}
\alias{HL.plot}
\title{Plots water level data and high/low tides extracted by HL()}
\description{
  Plots water level data and high/low tides extracted by HL(). Purpose is for quick and easy visual assessment of HL() output.
}
\usage{
  HL.plot(level, time, period = 13, phantom = TRUE, tides = "all")
}
\arguments{
  \item{level}{a numeric vector of water levels}

  \item{time}{a vector (numeric or POSIX*) indicating the time of water level measurements. Units must be minutes.}

  \item{period}{a single numeric or integer estimate of tidal period (full tidal cycle). Units must be hours.}

  \item{phantom}{a protective measure taken to prevent the inclusion of an artificial high or low tide at the end of the dataset. If the water level measurements end precisely at a low or high tide, this can be changed to FALSE.}

  \item{tides}{is used to optionally subset the output to include only high or low tides. This argument can be 'all' (default), 'H', or 'L'}

}
\value{
  \item{plot}{a plot of water levels, with red and blue dots superimposed on high and low tides.}
}
\examples{
HL.plot(level = NL_6min_2013[,2], time = NL_6min_2013[,1])
HL.plot(level = NL_6min_2013[1:1000,2], time = NL_6min_2013[1:1000,1]) 
}