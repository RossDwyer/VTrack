\name{PointsDirect_crocs}
\alias{PointsDirect_crocs}
\docType{data}
\title{
Points File Containing VR2 Locations on the Wenlock River in 2008 
}
\description{
This points file contains the locations of 20 VR2 receivers plus their corresponding detection radiuses for monitoring saltwater crocodiles on the Wenlock River in 2008. This points file corresponds with the \code{\link{crocs}} dataset
}
\usage{data(PointsDirect_crocs)}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{LOCATION }}{
a numeric vector containing the factory assigned receiver serial number (\code{Receiver S/N}) }
    \item{\code{LATITUDE }}{a numeric vector containing the location's latitude (decimal degrees)}
    \item{\code{LONGITUDE }}{a numeric vector containing the location's longitude (decimal degrees)}
    \item{\code{RADIUS }}{a numeric vector containing the detection radius for the location in meters}
  }
}
\details{
The coordinates are given in decimal degrees (WGS 84), detection radiuses are in meters.
}
\source{
\url{www.uq.edu.au/eco-lab/V-Track}
}

\examples{
# Load the points file for the Wenlock River
data(PointsDirect_crocs)
head(PointsDirect_crocs)

# Plot the locations of the receivers
par(mfrow=c(1,1),las=1,bty="l")
plot(PointsDirect_crocs$LONGITUDE,PointsDirect_crocs$LATITUDE,
     pch=10,cex=1,xlab="Longitude",ylab="Latitude")
}
\keyword{datasets}
