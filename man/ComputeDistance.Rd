\name{ComputeDistance}
\alias{ComputeDistance}
\title{
Compute the Distance Between Two Coordinates
}
\description{
This function computes the distance between two geographical coordinates. These locations must be in decimal degrees. 
}
\usage{
ComputeDistance(Lat1, Lat2, Lon1, Lon2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Lat1 }{
the latitude of the first coordinate 
}
  \item{Lat2 }{
the latitude of the second coordinate
}
  \item{Lon1 }{
the longitude of the first coordinate
}
  \item{Lon2 }{
the longitude of the second coordinate
}
}
\details{
Coordinates are given in decimal degrees (WGS 84)
}

\examples{
# Calculate the distance between two coordinates
ComputeDistance(-12.19506,-12.19477,141.8946,141.8980) 
}

\keyword{spatial}