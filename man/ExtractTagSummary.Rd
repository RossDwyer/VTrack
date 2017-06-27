\name{ExtractTagSummary}
\alias{ExtractTagSummary}
\title{
Function to Extract Summary Data for each Transmitter in the File 
}
\description{
This function extracts summary data for each transmitter in the File 
}
\usage{
ExtractTagSummary(sInputFile,sLocation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sInputFile }{
a data frame containing VTrack archive data, this archive is created using the ReadInputData function
}
  \item{sLocation }{the location at which we wish to extract our tag summary data (i.e. \code{RECEIVERID} or \code{STATIONNAME}). The default is \code{RECEIVERID}}
}


\value{
  \item{TRANSMITTERID }{a character vector containing the factory assigned transmitter tag id}
  \item{FIRSTDETECT }{a vector of class POSIXct of the time of the first location fix}
  \item{LASTDETECT }{a vector of class POSIXct of the time of the last location fix}
  \item{NODETECTS }{a numeric vector giving the number of transmitter location fixes}    
  \item{STARTREC }{a character vector containing the first location at which the transmitter was detected}
  \item{ENDREC }{a character vector containing the last location at which the transmitter was detected}
  \item{NORECS }{a numeric vector giving the number of unique receivers or stations at which the transmitter was detected}  
}

\author{Ross Dwyer}

\examples{
# Load the crocodile data in the VTrack 1.0 archive format
#  adding 10 hours to convert from UTC 
data(crocs)
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        fAATAMS=FALSE,
                        fVemcoDualSensor=FALSE,
                        dateformat = NULL,
                        sVemcoFormat='1.0')

# Extract summary table of transmitters in the file
ExtractTagSummary(Vcrocs)
}

\details{
distances are given in meters
}


\keyword{spatial}