\name{dur.bias}
\alias{dur.bias}
\title{
Calculate bias in flooding duration estimates
}
\description{
Seasonality in water levels imparts  bias to flooding duration datasets built from short-term water level 
data. \code{dur.bias} calculates and presents these biases in visual and tabular form.

Using data from a nearby, user-defined NOAA station, bias is estimated by comparing the relationship 
between flooding duration and elevation in datasets covering two time periods: (1) the time period of 
water level logger deployment, and (2) the reference time period (some number of years).
}
\usage{
dur.bias(data, station = 8518750, 
  ref.period = c("20120101", "20121231"), time = "GMT")
}
\arguments{
  \item{data}{
water level dataset. must have a time stamp column named 'datetime' with the first ten digits following 
the format \code{YYYY-MM-DD}. Different separators are fine, but the placement and number of digits for 
year, month, and day are critical. Date range must be  continuous, or the "estimated" curve won't be 
accurate (datasets with gaps won't work well).
}
  \item{station}{
name or number of NOAA station to be used for reference data. Ideally, use the station closest to where 
your water level data was collected. This is fed to \code{\link{noaa}} so must be compatible with 
\code{noaa}'s \code{station} argument.
}
  \item{ref.period}{
downloads NOAA tide data to set reference flooding-elevation relationship. This is used to evaluate 
accuracy of flooding duration estimates determined by water level deployment. Use of whole years is 
recommended. dates must be entered in format \code{YYYYMMDD}, in the form (c(start_date, end_date)).
}
  \item{time}{
time zone to download reference data. \code{GMT} is default; the NOAA website does not have all data 
available in all time zones.
}
}
\value{
(1) A summary of root mean square errors is printed, (2) a data frame of elevations, flooding duration 
estimates, and their associated bias estimates is saved as output, and (3) two plots are made, graphically 
showing the relationship between predicted and reference flooding duration curves.
}
\references{
http://wetlandsandr.wordpress.com/2014/09/14/measuring-bias-in-flooding-duration-estimates/
}
\seealso{
\code{\link{noaa}}, \code{\link{noaa.stations}}, \code{\link{fld.dur}}
}
