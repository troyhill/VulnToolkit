\name{fld.frq}
\alias{fld.frq}
\title{Calculates the percent of high tides flooding an elevation of interest.}
\description{
  Calculates the percent of high tides flooding an elevation of interest.
}
\usage{
  fld.frq(x, vec = x) 
}
\arguments{
  \item{x}{elevation of interest}
  
  \item{vex}{a numeric vector of high tide levels}
}
\value{
  \item{value}{the percent of measurements in 'vec' that fall above 'x'}
}