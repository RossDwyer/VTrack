\name{ExtractUniqueValues}
\alias{ExtractUniqueValues}
\title{
Extract Transmitters Found, or Receivers and Stations Used
}
\description{
Extract a list of the transmitters located, receivers used or stations used during the study.
}
\usage{
ExtractUniqueValues(sInputFile,iFieldToExtract)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sInputFile }{
a data frame containing \code{VTrack}-transformed tracking data
}
  \item{iFieldToExtract }{
numeric. Column number of \code{sInputFile} relating to field of interest (\code{TRANSMITTERID = 2}; \code{RECEIVERID = 5}; \code{STATIONNAME = 6})  
}
}

\author{
Ross Dwyer, Mathew Watts, Hamish Campbell
}

\examples{
data(crocs)
# Load the crocodile data in the VTrack archive format
#  adding 10 hours to convert from UTC 
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        dateformat = NULL,
                        sVemcoFormat='1.0')

# Extract list of transmitters from the crocs dataset
ExtractUniqueValues(Vcrocs,2)

# Extract list of receivers from the crocs dataset
ExtractUniqueValues(Vcrocs,5)
}

\keyword{extract}