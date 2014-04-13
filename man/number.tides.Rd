\name{number.tides}
\alias{number.tides}
\title{Numbers tidal cycles, flood tides, and ebb tides}
\description{
  Numbers tidal cycles, flood tides, and ebb tides in a set of water level data.
}
\usage{
  number.tides(data, datetime, hl) 
}
\arguments{
  \item{data}{dataframe to modify (containing water levels, time stamps)}
  
  \item{datetime}{date/time column from full dataset (used as 'time' argument in call to \code{HL()})}
  
  \item{hl}{output from \code{HL()}}
}
\value{
  \item{dataset}{the dataframe noted in \code{data}, with additional columns assigning a number to each tidal cycle, ebb tide, and flood tide.}
}
\examples{
# build high-low dataset
HL.NL <- HL(level = NL_6min_2013[,2], time = NL_6min_2013[,1])
# number tides in original
nos <- number.tides(data = NL_6min_2013, datetime = NL_6min_2013[,1], HL.NL)
head(nos)
}