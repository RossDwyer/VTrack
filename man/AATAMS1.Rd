\name{AATAMS1}
\alias{AATAMS1}
\docType{data}
\title{
Passive Acoustic Monitoring of one animal in the AATAMS format
}
\description{
This AATAMS dataset contains the relocations of one animal monitored between 08 January 2010 and 15 April 2011. Data supplied by Andrew Boomer from AATAMS-IMOS.

}
\usage{data(AATAMS1)}
\format{
  A data frame with 2735 observations on the following 10 variables.
  \describe{
    \item{\code{timestamp }}{a vector of type POSIXct in Co-ordinated Universal Time (UTC) Greenwich Mean Time}
    \item{\code{station.name }}{a character vector specifying the user-defined location for a particular deployment. This is usually assigned and recorded in the receivers memory in VUE before receiver deployment. Multiple receivers may be associated with the same \code{station.name}}
    \item{\code{latitude }}{a numeric vector containing the location's latitude (decimal degrees)}
    \item{\code{longitude }}{a numeric vector containing the location's longitude (decimal degrees)}
    \item{\code{receiver.ID }}{a character vector specifying the unique identity of each receiver according to their model and serial number (i.e. \code{VR2W-101731})}
    \item{\code{tag.ID }}{a character vector containing either a combination of the code space and factory assigned transmitter ID number (i.e. \code{346})}
    \item{\code{species }}{the species being studied. \code{NA} suggests that no species name was supplied}
    \item{\code{uploader }}{a character vector giving the identity of person who uploaded the data}
    \item{\code{transmitter.ID }}{a numeric vector containing the factory assigned transmitter serial number, \code{A69-1303-7796}}
    \item{\code{organisation }}{a character vector giving the organisation to which the data belongs. Data belongs to \code{AATAMS-IMOS}}
  }
}
\details{
The coordinates are given in decimal degrees (WGS 84), time is GMT+10hrs. 
}
\source{
\url{http://imos.org.au/home.html}
}

\examples{
# Load the data and print the first few rows of the data frame
data(AATAMS1)
head(AATAMS1)
}
\keyword{datasets}
