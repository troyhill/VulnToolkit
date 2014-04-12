\name{number.tides}
\alias{number.tides}
\title{Numbers tidal cycles, flood tides, and ebb tides in a set of water level data.}
\description{
  Numbers tidal cycles, flood tides, and ebb tides in a set of water level data.
}
\usage{
  number.tides(data, datetime, hl) 
}
\arguments{
  \item{data}{dataframe containing water level measurements}
  
  \item{datetime}{date/time column from full dataset (used as 'time' argument in call to \code{HL()})}
  
  \item{hl}{output from \code{HL()}}
}
\value{
  \item{dataset}{the dataframe noted in \code{data}, with additional columns assigning a number to each tidal cycle, ebb tide, and flood tide.}
}