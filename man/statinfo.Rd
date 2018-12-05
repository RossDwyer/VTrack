\name{statinfo}
\alias{statinfo}
\docType{data}
\title{
Points File Containing VR2 Locations 
}
\description{
This points file contains the locations of X VR2-W receivers corresponding with the \code{\link{crocs}} dataset
}
\usage{data(statinfo)}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{project_name }}{
a numeric vector containing the factory assigned receiver serial number (\code{Receiver S/N}) }
    \item{\code{installation_name }}{
a numeric vector containing the factory assigned receiver serial number (\code{Receiver S/N}) }
    \item{\code{station_name }}{
a numeric vector containing the factory assigned receiver serial number (\code{Receiver S/N}) }
    \item{\code{receiver_name }}{
a numeric vector containing the factory assigned receiver serial number (\code{Receiver S/N}) }
    \item{\code{deploymentdatetime_timestamp }}{
a numeric vector containing the factory assigned receiver serial number (\code{Receiver S/N}) }
    \item{\code{recoverydatetime_timestamp }}{
a numeric vector containing the factory assigned receiver serial number (\code{Receiver S/N}) }
    \item{\code{status }}{
a numeric vector containing the factory assigned receiver serial number (\code{Receiver S/N}) }
    \item{\code{station_longitude }}{a numeric vector containing the location's longitude (decimal degrees)}
    \item{\code{station_latitude }}{a numeric vector containing the location's latitude (decimal degrees)}
    \item{\code{imos_device }}{a numeric vector containing the detection radius for the location in meters}
  }
}
\details{
The coordinates are given in decimal degrees (WGS 84), detection radiuses are in meters.
}
\source{
\url{www.uq.edu.au/eco-lab/V-Track}
}

\examples{
data(statinfo)
head(statinfo)


}
\keyword{statinfo}
