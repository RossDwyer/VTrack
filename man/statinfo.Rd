\name{statinfo}
\alias{statinfo}
\docType{data}
\title{
Example dataset containing receiver station metadata associated with \code{IMOSdata} and \code{VEMCOdata} datasets
}
\description{
This dataset contains an example of receiver array information required for the functioning of the ATT operations. The example array is located at Heron and One Tree Island, Australia, and was used to track Tiger sharks (see \code{IMOSdata} or \code{VEMCOdata}). The data is in the default format used by the IMOS ATF containing the metadata associated with receiver deployments.
}
\usage{data(statinfo)}
\format{
  A data frame with 498 observations on the following 10 variables.
  \describe{
    \item{\code{project_name }}{ a factor containing project name under which the receiver was registered under (e.g. AATAMS One Tree Island). }
    \item{\code{installation_name }}{a factor containing the name of installation each reciever is part of. An installation typically consists of multiple receiving stations (e.g. One Tree Island). }
    \item{\code{station_name }}{a factor containing the name of unique receiver station. Acoustic receivers typically gets deployed multiple times, or cycled through at the same station (e.g. OT-7). Required field. }
    \item{\code{receiver_name }}{a factor containing the name of acoustic receiver, combines receiver model with its serial number (e.g. VR2W-123456). Required field. }
    \item{\code{deploymentdatetime_timestamp }}{a vector of type POSIXct in Coordinated Universal Time (UTC)/ Greenwich Mean Time contining date and time when the receiver was deployed (yyyy-mm-dd HH:MM:SS). Required field. }
    \item{\code{recoverydatetime_timestamp }}{a vector of type POSIXct in Coordinated Universal Time (UTC)/ Greenwich Mean Time contining date and time when the receiver was recovered and not redeployed at this station (yyyy-mm-dd HH:MM:SS). Required field. }
    \item{\code{status }}{a character vector indicating the current status of the receiver (e.g. DEPLOYED, DAMAGED, LOST, RECOVERED, etc). }
    \item{\code{station_longitude }}{a numeric vector containing the longitude of the receiver station in decimal degrees. }
    \item{\code{station_latitude }}{a numeric vector containing the latitude of the receiver station in decimal degrees. }
    \item{\code{imos_device }}{a logical vector indicating if the device was provided and/or is maintained by the IMOS ATF. }
  }
}
\details{
The coordinates are given in decimal degrees WGS 84, time is in UTC/GMT.
}
\source{
\url{https://animaltracking.aodn.org.au}
}

\examples{
## load the data and print the first few rows of the data frame
data(statinfo)
head(statinfo)


}
\keyword{statinfo}
