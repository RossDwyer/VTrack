\name{GenerateLeastCostDistance}
\alias{GenerateLeastCostDistance}
\title{
Converts a Points File into a Distance Matrix Using an Transition Layer Generated from a Rasterised version of the Study Area
}
\description{
This function calculates the distance between a set of geographical coordinates and generates a matrix containing the distances between each of the points.
}
\usage{
GenerateLeastCostDistance(sPointsFile,sTransition)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sPointsFile }{
a dataframe containing the \code{LOCATION} (i.e. the STATIONNAME or the RECEIVERID), the coordinates and the detection \code{RADIUS} in meters. This should be in the format \code{LOCATION}, \code{X}, \code{Y}, \code{RADIUS}. Note - in this beta version the code only works with Cartesian coordinate systems (i.e. meters not degrees)
}
  \item{sTransition }{
a Transition object generated from a RasterLayer using the gdistance R package
}

\value{
a 2x2 matrix containing the pairwise direct \code{DISTANCE} between each \code{LOCATION} minus the detection \code{RADIUS}. Distances are returned in kilometers
}
\author{
Ross Dwyer
}

\seealso{
\code{\link{GenerateCircuitousDistance}}
}

\examples{
# Load the points file
#data(PointsLeastCost_crocs)
# Generate the Least Cost distance matrix
#RasterDM <- GenerateLeastCostDistance(PointsLeastCost_crocs)

\keyword{spatial}