\name{VEMCOdata}
\alias{VEMCOdata}
\docType{data}
\title{
Passive Acoustic Monitoring of 
}
\description{
This VEMCO dataset in the Default format containing the relocations of .
}
\usage{data(VEMCOdata)}
\format{
  A data frame with 11229 observations on the following 14 variables.
  \describe{
    \item{\code{Date.and.Time..UTC. }}{a vector of type POSIXct in Co-ordinated Universal Time (UTC)/ Greenwich Mean Time}
    \item{\code{Receiver }}{a factor specifying the unique identity of each receiver according to their model and serial number. i.e. \code{VR2W-114582}}
    \item{\code{Transmitter }}{a factor giving the identity of each transmitter, \code{A69-1303-64957}}
    \item{\code{Transmitter.Name }}{a character vector containing the user defined transmitter name.}
    \item{\code{Transmitter.Serial }}{a numeric vector containing the factory assigned transmitter serial number.}
    \item{\code{Sensor.Value }}{as this study included coded tags only, no environmental sensor data are present in this vector}
    \item{\code{Sensor.Unit }}{as this study included coded tags only, no sensor units are present in this vector}
    \item{\code{Station.Name }}{an optional character vector specifying the user-defined location for a particular deployment. This is usually assigned and recorded in the receivers memory in VUE before receiver deployment. Multiple receivers may be associated with the same station name.}
    \item{\code{Latitude }}{a numeric vector containing the location's latitude in decimal degrees}
    \item{\code{Longitude }}{a numeric vector containing the location's longitude in decimal degrees}
  }
}
\details{
The coordinates are given in decimal degrees WGS 84, time is GMT +10 hrs
}
\source{
\url{http://imos.org.au/facilities/animaltracking/}
}

\examples{
#load the data and print the first few rows of the data frame
data(VEMCOdata)
head(VEMCOdata)
}
\keyword{datasets}
