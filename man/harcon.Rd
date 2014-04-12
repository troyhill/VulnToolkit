\name{harcon}
\alias{harcon}
\title{Scrapes harmonic constituent data from NOAA CO-OPS website.}
\description{
  Scrapes harmonic constituent data from NOAA CO-OPS website. Requires internet connection.
}
\usage{
  harcon(station)
}
\arguments{
  \item{station}{station name or ID number, available on CO-OPS site (http://co-ops.nos.noaa.gov/stations.html?type=Water+Levels).}
}
\value{
  \item{dataset}{a dataframe of harmonic constituents and their associated phases, amplitudes, and speeds.}
}