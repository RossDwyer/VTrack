\name{PointsCircuitous_crocs}
\alias{PointsCircuitous_crocs}
\docType{data}
\title{
Points File Containing VR2 Locations on the Wenlock River in 2008 with Waypoints Connecting Receivers
}
\description{
This points file contains the locations of 20 VR2 receivers plus their corresponding detection radiuses for monitoring saltwater crocodiles on the Wenlock River in 2008. When receivers have an obstructed line of view to landscape features (i.e. an island or a bend in the river) waypoints were added to facilitate the course of the shortest path. This points file corresponds with \code{\link{crocs}}.
}
\usage{data(PointsCircuitous_crocs)}
\format{
  A data frame with 149 observations on the following 4 variables.
  \describe{
    \item{\code{LOCATION }}{a numeric vector containing the receiver serial number (i.e. \code{RECEIVERID})}
    \item{\code{LATITUDE }}{a numeric vector containing the location's latitude in decimal degrees}
    \item{\code{LONGITUDE }}{a numeric vector containing the location's longitude in decimal degrees}
    \item{\code{RADIUS }}{a numeric vector containing the detection radius for the receiver in meters}
  }
}
\details{
The coordinates are given in decimal degrees WGS 84, detection radiuses are in meters.
}
\source{
\url{www.uq.edu.au/eco-lab/V-Track}
}
\examples{
# Load the points file for the Wenlock River
data(PointsCircuitous_crocs)
head(PointsCircuitous_crocs)
receiversonly <- na.omit(PointsCircuitous_crocs)

# Plot the locations of the receivers plus the waypoints
par(mfrow=c(1,1),las=1,bty="l")
plot(PointsCircuitous_crocs$LONGITUDE, PointsCircuitous_crocs$LATITUDE,
     pch=1,cex=0.5,col="grey",xlab="Longitude",ylab="Latitude")
points(receiversonly$LONGITUDE,receiversonly$LATITUDE,cex=1,pch=10)
}
\keyword{datasets}
