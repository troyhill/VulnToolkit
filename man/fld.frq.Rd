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
  
  \item{vec}{a numeric vector of high tide levels}
}
\value{
  \item{value}{the percent of high tides in 'vec' that fall above 'x'}
}
\examples{
# get a dataset of high/low tides
HT.NL <- HL(level = NL_6min_2013[,2], time = NL_6min_2013[,1], tides = "H")
a <- fld.frq(0.9117, HT.NL[,1]); a # flooding frequency at MHW
a * length(HT.NL[,1]) # convert to tides per year

b <- fld.frq(0, HT.NL[,1]); b # flooding frequency at MLLW
b * length(HT.NL[,1]) # tides per year

elev.frq <- data.frame(elev = seq(from = 0, to = 1.5, by = 0.005))
elev.frq$frq <- fld.dur(elev.frq$elev, HT.NL[,1]) * length(HT.NL[,1])

plot(elev.frq$frq ~ elev.frq$elev, pch = 19, ylab = "flooding frequency (tides per year)", xlab = "elevation (m; MLLW)")
}