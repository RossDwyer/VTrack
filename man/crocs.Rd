\name{crocs}
\alias{crocs}
\docType{data}
\title{
Passive Acoustic Monitoring of Saltwater Crocodiles 
}
\description{
This VEMCO dataset in the 1.0 format  contains the relocations of 3 saltwater crocodiles monitored between 09 September 2008 to 31 December 2008 on the Wenlock River, Cape York, Queensland, Australia. Data supplied H. Campbell from the School of Biological Sciences, The University of Queensland, Queensland, Australia.
}
\usage{data(crocs)}
\format{
  A data frame with 11229 observations on the following 14 variables.
  \describe{
    \item{\code{Date.Time }}{a vector of type POSIXct in Co-ordinated Universal Time (UTC)/ Greenwich Mean Time}
    \item{\code{Code.Space }}{a character vector containing the type of coding scheme used for the particular tag type. This unique identifier encompasses all the information required for a receiver to detect and decode that particular transmitter (e.g. A69-0001 is an acoustic transmitter operating at a frequency of 69Hz and has 0001 as a unique number identifier).}
    \item{\code{ID }}{a numeric vector giving the identity of each transmitter, \code{94,99,139,138}}
    \item{\code{Sensor.1 }}{a numeric vector giving the value of the environmental sensor such as temperature or depth at the time of detection}
    \item{\code{Units.1 }}{a factor with levels \code{m} and degrees C}
    \item{\code{Sensor.2 }}{as this study included coded tags only, no environmental sensor data are present in this vector}
    \item{\code{Units.2 }}{as this study included coded tags only, no sensor units are present in this vector}
    \item{\code{Transmitter.Name }}{a character vector containing user defined animal names}
    \item{\code{Transmitter.S.N }}{a numeric vector containing the factory assigned transmitter serial number}
    \item{\code{Receiver.Name }}{a factor specifying the unique identity of each receiver according to their model and serial number. i.e. \code{VR2W-103548}}
    \item{\code{Receiver.S.N }}{a numeric vector containing the factory assigned receiver serial number i.e. \code{1035481}}
    \item{\code{Station.Name }}{an optional character vector specifying the user-defined location for a particular deployment. This is usually assigned and recorded in the receivers memory in VUE before receiver deployment. Multiple receivers may be associated with the same station name.}
    \item{\code{Station.Latitude }}{a numeric vector containing the location's latitude in decimal degrees}
    \item{\code{Station.Longitude }}{a numeric vector containing the location's longitude in decimal degrees}
  }
}
\details{
The coordinates are given in decimal degrees WGS 84, time is GMT +10 hrs
}
\source{
\url{www.uq.edu.au/eco-lab/V-Track}
}

\examples{
#load the croc dataset (Vemco VUE 1.0 format) and print the first few rows of the data frame
data(crocs)
head(crocs)
}
\keyword{datasets}
