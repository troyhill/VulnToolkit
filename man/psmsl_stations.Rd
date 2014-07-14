\name{psmsl.stations}
\alias{psmsl.stations}
\title{
Generates a list of active and historic tide stations hosted by the Permanent Service for Mean Sea Level (www.psmsl.org)
}
\description{
This function scrapes data listings from www.psmsl.org.
}
\usage{
psmsl.stations(type = "RLR", country = "all", sort.by = "country")
}
\arguments{
  \item{type}{
data quality class; can be 'metric' or 'RLR' - see www.psmsl.org for documentation
}
  \item{country}{
if desired, the full list of stations can be filtered by up to three alphabetical characters. Specific country codes can be entered ("USA"), or abbreviated codes in case the user isn't sure of the country code ("U"; "US"). Upper case and lower case codes are both acceptable. Default is 'all' stations.
}
  \item{sort.by}{
the criterion for sorting the final dataframe.  By default, output is sorted alphabetically by country code. Any column name can be used for sorting: 'name', 'ID', 'lat', 'long', 'GLOSS_ID', 'country', 'date', 'coastline', or 'number'
}
}
\value{
A data frame \code{\link{data.frame}} containing all Permanent Service for Mean Sea Level stations meeting country code criterion. Data frame is sorted by the column specified in argument 'sort.by'
}
\references{
http://wetlandsandr.wordpress.com/
}
\seealso{
\code{\link{psmsl}}
}
\examples{
# examples require internet connection
# stn.df <- psmsl.stations()
# stn.df2 <- psmsl.stations(country = "USA", sort.by = "date")

}
\keyword{ ~IO }