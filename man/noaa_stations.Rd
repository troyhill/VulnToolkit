\name{noaa.stations}
\alias{noaa.stations}
\title{
Prints active NOAA stations
}
\description{
\code{noaa.stations} returns active NOAA CO-OPS tide stations.
}
\details{
This code returns a dataframe showing all active stations in the NOAA CO-OPS network.
This can be used to identify stations for use in \code{noaa()} (see example below).
}
\usage{
noaa.stations(state = "all")
}
\arguments{
  \item{state}{limits printed records to a state or states of interest. Default is to show 'all' stations.
}
}
\value{
A dataframe with station names, states/territories, and station numbers. If a
vector of states is provided, some of which are invalid, a dataframe will be produced for the
valid criteria and invalid criteria will be noted.
}
\seealso{
NOAA CO-OPS site: http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels 
Historic stations which can also be accessed by \code{noaa()}: 
http://co-ops.nos.noaa.gov/stations.html
}
\examples{
# examples require internet connection
# a <- noaa.stations() # all active stations
# b <- noaa.stations(state = "MA") 
# c <- noaa.stations(state = c("RI", "CT"))
# d <- noaa.stations(state = c("OR", "RI", "MA", "Germany", "Pluto")) # two erroneous entries 
# highlow <- noaa(station = d[1,1], begindate = 20130101)
}
