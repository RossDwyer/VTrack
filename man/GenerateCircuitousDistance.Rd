\name{GenerateCircuitousDistance}
\alias{GenerateCircuitousDistance}
\title{
Converts a Points File into a Distance Matrix Using the Circuitous Distances Along a Series of Receivers or Stations
}
\description{
This function calculates the straight line distance beween a set of geographical coordinates and generates a matrix containing the distances between each of the locations (i.e. receivers/stations) minus the detection radius. This function works in series through a set of locations and may contain waypoints to create indirect paths.
}
\usage{
GenerateCircuitousDistance(sPointsFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sPointsFile }{
a dataframe containing the \code{LOCATION} (i.e. the STATIONNAME or the RECEIVERID), the coordinates and the detection \code{RADIUS} in meters. This should be in the format \code{LOCATION}, \code{LATITUDE}, \code{LONGITUDE}, \code{RADIUS}. Waypoints connecting receivers/stations in series should be located between the relevent locations and have a \code{LOCATION = 0}
}
}
\value{
  \item{DM}{
a 2x2 matrix containing the pairwise circuitous \code{DISTANCE} between each \code{LOCATION} minus the detection \code{RADIUS}. Distances are returned in kilometers
}
}

\author{
Ross Dwyer, Mathew Watts, Hamish Campbell
}

\seealso{
\code{\link{GenerateDirectDistance}}
}
\examples{

# Load the points file
data(PointsCircuitous_crocs)

# Generate the Circuitous Distance Matrix
CircuitousDM <- GenerateCircuitousDistance(PointsCircuitous_crocs)

# Now plot example of how circuitous distances between receivers were generated
# In this example an individual must follow the course of the river in order to
#   move between receivers
par(mfrow=c(1,1),las=1,bty="l")
plot(PointsCircuitous_crocs$LONGITUDE,PointsCircuitous_crocs$LATITUDE,
    pch=1,cex=0.5,col="grey",xlab="Longitude",ylab="Latitude")
lines(PointsCircuitous_crocs$LONGITUDE,PointsCircuitous_crocs$LATITUDE,
      col="grey",lwd=0.3,lty=3)

Receiversonly <- na.omit(PointsCircuitous_crocs)
points(Receiversonly$LONGITUDE,Receiversonly$LATITUDE,pch=10,cex=1)
}

\keyword{spatial}