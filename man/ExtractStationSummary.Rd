\name{ExtractStationSummary}
\alias{ExtractStationSummary}
\title{
Extended Function to Extract Summary Data for each Station Name in the File 
}
\description{
This function extracts summary data for each station name in the file
}
\usage{
ExtractStationSummary(sInputFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sInputFile }{
a data frame containing VTrack archive data, this archive is created using the ReadInputData function
}
}

\value{
  \item{RECEIVERID }{a character vector containing the factory assigned receiver serial number}
  \item{STATIONNAME }{a character vector containing the user defined station name}
  \item{FIRSTDETECT }{a vector of class POSIXct of the time of the first location fix}
  \item{NODETECTS }{a numeric vector giving the number of transmitter location fixes}  
  \item{LASTDETECT }{a vector of class POSIXct of the time of the last location fix}
  \item{NOTRANSMITTER }{a numeric vector giving the number of unique transmitter ids detected}  
}
\author{Ross Dwyer}


\examples{
data(crocs)
# Load the crocodile data in the VTrack archive format
#  adding 10 hours to convert from UTC 
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        dateformat = NULL,
                        sVemcoFormat='1.0')

# Extract summary table of receiver sstations in the file
ExtractStationSummary(Vcrocs)
}

\details{
duration given in days. 
}


\keyword{spatial}