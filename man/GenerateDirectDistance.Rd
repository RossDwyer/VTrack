\name{GenerateDirectDistance}
\alias{GenerateDirectDistance}
\title{
Converts a Points File into a Distance Matrix Using Direct Distances Between Receivers or Stations
}
\description{
This function calculates the straight line distance between a set of geographical coordinates and generates a matrix containing the distances between each of the ponts (i.e. receivers) minus the detection radius.
}
\usage{
GenerateDirectDistance(sPointsFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sPointsFile }{
a dataframe containing the \code{LOCATION} (i.e. the STATIONNAME or the RECEIVERID), the coordinates and the detection \code{RADIUS} in meters. This should be in the format \code{LOCATION}, \code{LATITUDE}, \code{LONGITUDE}, \code{RADIUS}
}
}

\value{
a 2x2 matrix containing the pairwise direct \code{DISTANCE} between each \code{LOCATION} minus the detection \code{RADIUS}. Distances are returned in kilometers
}
\author{
Ross Dwyer, Mathew Watts, Hamish Campbell
}

\seealso{
\code{\link{GenerateCircuitousDistance}}
}

\examples{
# Load the points file
data(PointsDirect_crocs)
# Generate the direct distance matrix
DirectDM <- GenerateDirectDistance(PointsDirect_crocs)

# Now plot example of how direct distances between receivers were generated
# In this example there are no structural boundary preventing an individual from 
#   moving between receivers
par(mfrow=c(1,1),las=1,bty="l")
plot(PointsDirect_crocs$LONGITUDE,PointsDirect_crocs$LATITUDE,pch=10,cex=1,
     xlab="Longitude",ylab="Latitude")
for(i in 1:length(PointsDirect_crocs$LONGITUDE))
  {
   lines(PointsDirect_crocs$LONGITUDE[c(1,i)],PointsDirect_crocs$LATITUDE[c(1,i)],
         lwd=0.3,col="grey",lty=3)
   }
points(PointsDirect_crocs$LONGITUDE,PointsDirect_crocs$LATITUDE,pch=10,cex=1)
}

\keyword{spatial}