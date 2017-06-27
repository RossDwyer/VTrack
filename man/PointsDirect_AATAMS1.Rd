\name{PointsDirect_AATAMS1}
\alias{PointsDirect_AATAMS1}
\docType{data}
\title{
Points File Containing VR2 Locations For AATAMS1
}
\description{
This points file contains the locations of two acoustic stations plus their corresponding detection radiuses for monitoring x on y in 2009. This points file corresponds with \code{\link{AATAMS1}}
}
\usage{data(PointsDirect_AATAMS1)}
\format{
  A data frame with 2 observations on the following 4 variables.
  \describe{
    \item{\code{LOCATION }}{a factor containing the station name}
    \item{\code{LATITUDE }}{a numeric vector containing the location's latitude (decimal degrees)}
    \item{\code{LONGITUDE }}{a numeric vector containing the location's longitude (decimal degrees)}
    \item{\code{RADIUS }}{a numeric vector the detection radius for the location in meters}
  }
}
\details{
The coordinates are given in decimal degrees (WGS 84), detection radiuses are in meters.
}
\source{
url{http://imos.org.au/home.html}
}

\examples{
# Load the points file for the AATAMS1 dataset
data(PointsDirect_AATAMS1)
head(PointsDirect_AATAMS1)
}
\keyword{datasets}
