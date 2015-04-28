\name{psmsl}
\alias{psmsl}
\title{
Download MSL data from www.psmsl.org
}
\description{
\code{psmsl} imports data stewarded by the Permanent Service for Mean Sea Level.
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
# example requires internet connection
# battery <- psmsl()
<<<<<<< HEAD
# stations <- psmsl(station = c(1372, 12), interval = "monthly")}
=======
# stations <- psmsl(station = c(1372, 12), interval = "monthly")
}
\keyword{ ~IO }
>>>>>>> d0e32ede8235fca8b8a3e150d38eeaada1b1f302
