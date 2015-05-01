\name{fld.frq}
\alias{fld.frq}
\title{Flooding frequency}
\description{
  Calculates the frequency of high tides flooding an elevation of interest.
}
\usage{
  fld.frq(z, ht,  units = "percent") 
}
\arguments{
  \item{z}{elevation of interest}
  
  \item{ht}{a numeric vector of high tide levels}
  
  \item{units}{units for output. Default is \code{percent}; the proportion of tides flooding elevation \code{x}. Alternatively, \code{tides} will return the number of flooding tides in the dataset.}
}
\value{
  \item{value}{the number or percent of high tides in \code{ht} that fall above \code{z}}
}
\examples{
# get a dataset of high/low tides
data(NL_6min_2013)
HT.NL <- HL(level = NL_6min_2013[, 2], time = NL_6min_2013[, 1], tides = "H")

# number of flooding tides at MHW
a <- fld.frq(0.9117, HT.NL[, 1], units = "tides")

# flooding tides as a percentage of all tides in time period
a / length(HT.NL[, 1]) 

# as a check
fld.frq(0.9117, HT.NL[,1], units = "percent") 

# fraction of year covered by dataset
b <- as.numeric((NL_6min_2013[nrow(NL_6min_2013), 1] - NL_6min_2013[1, 1])) / 365.242 

elev.frq <- data.frame(elev = seq(from = 0, to = 1.5, by = 0.005))

# error thrown if units = "tides" and length(x) > length(ht)
fld.frq(elev.frq$elev, HT.NL[, 1], units = "tides") 

# but there's a work-around
elev.frq$frq <- fld.frq(elev.frq$elev, HT.NL[, 1], units = "percent") * length(HT.NL[, 1]) / b 

plot(elev.frq$frq ~ elev.frq$elev, pch = 19, 
  ylab = "flooding frequency (tides per year)", xlab = "elevation (m; MLLW)")
}
