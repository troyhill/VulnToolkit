\name{psmsl}
\alias{psmsl}
\title{
Downloads tide data from www.psmsl.org
}
\description{
This function scrapes data stewarded by the Permanent Service for Mean Sea Level, according to user-defined criteria.
}
\usage{
psmsl(station = 12, type = "RLR", interval = "annual")
}
\arguments{
  \item{station}{
station name or ID, or a vector of station names/IDs. Elements can be a character string (must match actual station name identically), or numeric station ID (no quotes: i.e., 12 rather than "12"). Use \code{\link{psmsl.stations}} to find stations, or check www.psmsl.org. If multiple stations are included, their data is combined using rbind, making a long (rather than wide) dataset. Default station is the Battery, in New York City.
}
  \item{type}{
data quality class; can be 'metric' or 'RLR'. See www.psmsl.org for documentation.
}
  \item{interval}{
time interval over which mean sea level is calculated. Can be 'monthly' or 'annual'.}
}
\value{
A data frame \code{\link{data.frame}} containing the requested Permanent Service for Mean Sea Level data.
}
\references{
http://wetlandsandr.wordpress.com/
}
\seealso{
\code{\link{psmsl.stations}}
}
\examples{
# examples require internet connection
# battery <- psmsl()
# stations <- psmsl(station = c(1372, 12), interval = "monthly")
}
\keyword{ ~IO }