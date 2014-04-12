\name{fld.dur}
\alias{fld.dur}
\title{Calculates the percent of time an elevation is submerged.}
\description{
  Calculates the percent of time an elevation is submerged
}
\usage{
  fld.dur(x, vec = x) 
}
\arguments{
  \item{x}{elevation of interest}
  
  \item{vex}{a numeric vector of water levels}
}
\value{
  \item{value}{the percent of measurements in 'vec' that fall above 'x'}
}