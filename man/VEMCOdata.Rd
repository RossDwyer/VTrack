\name{VEMCOdata}
\alias{VEMCOdata}
\docType{data}
\title{
Passive Acoustic Monitoring of 
}
\description{
This VEMCO dataset is an example dataset containing 11 tagged Tiger sharks tracked at Heron and One Tree Island, Australia. The data is in the default format used by VEMCO VUE software containing the relocations of coded tags across an array of acoustic hydrophones.
}
\usage{data(VEMCOdata)}
\format{
  A data frame with 78753 observations on the following 10 variables.
  \describe{
    \item{\code{Date.and.Time..UTC. }}{a vector of type POSIXct in Co-ordinated Universal Time (UTC)/ Greenwich Mean Time}
    \item{\code{Receiver }}{a factor specifying the unique identity of each receiver according to their model and serial number. e.g. \code{VR2W-114582}}
    \item{\code{Transmitter }}{a factor giving the identity of each transmitter, e.g. \code{A69-1303-64957}}
    \item{\code{Transmitter.Name }}{a character vector containing the user defined transmitter name.}
    \item{\code{Transmitter.Serial }}{a numeric vector containing the factory assigned transmitter serial number.}
    \item{\code{Sensor.Value }}{a numeric vector containing values from VEMCO sensor tags (e.g. pressure/depth, acceleration, temperature).}
    \item{\code{Sensor.Unit }}{a character vector conatining the units pertaining to the sensor value (e.g. m, ms-2, degrees celcius).}
    \item{\code{Station.Name }}{an optional character vector specifying the user-defined location for a particular deployment. This is usually assigned and recorded in the receivers memory in VUE before receiver deployment. Multiple receivers may be associated with the same station name.}
    \item{\code{Latitude }}{a numeric vector containing the location's latitude in decimal degrees}
    \item{\code{Longitude }}{a numeric vector containing the location's longitude in decimal degrees}
  }
}
\details{
The coordinates are given in decimal degrees WGS 84, time is in UTC/GMT
}

\examples{
#load the data and print the first few rows of the data frame
data(VEMCOdata)
head(VEMCOdata)
}
\keyword{datasets}
