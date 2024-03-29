% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psmsl.R
\name{psmsl}
\alias{psmsl}
\title{Download sea level data from Permanent Service for Mean Sea Level}
\usage{
psmsl(station = 12, type = "RLR", interval = "annual")
}
\arguments{
\item{station}{name or ID, or a vector of station names/IDs. Elements can be 
a character string (must match actual station name identically), or numeric 
station ID (no quotes: i.e., 12 rather than "12"). Use \code{\link{psmsl.stations}} 
to find stations, or check \url{http://www.psmsl.org}. If multiple stations are included, their 
data is combined using rbind, making a long (rather than wide) dataset. Default 
station is the Battery, in New York City.}

\item{type}{data quality class; can be 'metric' (default; data has been rigorously 
standardized) or 'RLR'. See \url{http://www.psmsl.org} for documentation.}

\item{interval}{time interval over which mean sea level is calculated. Can be 
'monthly' or 'annual'.}
}
\value{
A data frame \code{data.frame} containing the requested Permanent Service for Mean Sea Level data.
}
\description{
\code{psmsl} imports monthly/annual tide data from stations around the world. Dataset compiled and stewarded by the Permanent Service for Mean Sea Level (\url{http://www.psmsl.org}).
}
\examples{
\dontrun{
battery  <- psmsl()
stations <- psmsl(station = c(1372, 12), interval = "monthly")
}
}
\seealso{
\code{\link{psmsl.stations}} can be used to find stations
}
