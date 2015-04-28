\name{fld.depth}
\alias{fld.depth}
\title{Calculates flooding depth above an elevation of interest}
\description{
  Flooding depths are calculated from water level or tide data. If water levels are used, the median (or other percentile) flooding depth is calculated based on all observations of flooded conditions (when water depth is equal to or greater than the elevation of interest). If a high/low tide dataset is used, flooding depth percentiles will be just for high tides that flood the selected elevation. The latter case includes only peak high water levels, and so will yield greater flooding depths for the same elevation.
}
\usage{
fld.depth(level, elevation, percentile = 0.5)
}
\arguments{
  \item{level}{a numeric vector of water levels or tide data}

   \item{elevation}{elevation(s) of interest (marsh platform, MHW). A vector of elevations is accepted. Elevation should be in the same vertical datum as \code{level}}

  \item{percentile}{the percentile(s) to calculate (median flooding depth is calculated by default). A vector of percentiles is accepted. }
}
\value{
\item{output}{if 'percentile' argument is of length one, the value(s) returned are the corresponding flooding percentiles for the elevation(s) of interest. To ensure clarity when more than one 'percentiles' are sought, in those cases a dataframe is output to report elevations and flooding depths.}

}
\examples{
data(NL_6min_2013)
MHW <- 0.9117  # New London MHW in 2013: 0.9117 m relative to MLLW
fld.depth(level = NL_6min_2013[, 2], elevation = MHW, 
  percentile = 0.5)

fld.depth(level = NL_6min_2013[, 2], elevation = MHW, 
  percentile = c(0.25, 0.5, 0.75))

fld.depth(level = NL_6min_2013[, 2], elevation = c(0, MHW, 1.5), 
  percentile = 0.75)

fld.depth(level = NL_6min_2013[, 2], elevation = c(0, MHW, 1.5), 
  percentile = c(0.25, 0.5, 0.75))}
