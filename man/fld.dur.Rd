\name{fld.dur}
\alias{fld.dur}
\title{Calculates the percent of time an elevation is submerged}
\description{
  Calculates the percent of time an elevation is submerged
}
\usage{
  fld.dur(x, vec = x) 
}
\arguments{
  \item{x}{elevation of interest}
  
  \item{vec}{a numeric vector of water levels}
}
\value{
  \item{value}{the decimal fraction of measurements in 'vec' that fall above 'x'}
}
\examples{
a <- fld.dur(0.9117, NL_6min_2013[,2]); a # flooding duration at MHW
a * length(NL_6min_2013[,2]) / 10 # convert to hours per year

b <- fld.dur(0, NL_6min_2013[,2]); b # flooding duration at MLLW
b * length(NL_6min_2013[,2]) / 10 # hours per year

elev.dur <- data.frame(elev = seq(from = -0.5, to = 1.25, by = 0.005))
elev.dur$dur <- fld.dur(elev.dur$elev, NL_6min_2013[,2]) * length(NL_6min_2013[,2]) / 10 

plot(elev.dur$dur ~ elev.dur$elev, pch = 19, ylab = "flooding duration (hours per year)", xlab = "elevation (m; MLLW)")
}