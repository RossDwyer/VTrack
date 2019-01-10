\name{NonResidenceExtractId}
\alias{NonResidenceExtractId}
\title{
Extract the Non-residence Events, the Corresponding Distance Moved and the Rate of Movement 
}
\description{
This function creates a \code{nonresidences} data frame from a \code{residences} event data frame and a optional distance matrix (\code{sDistanceMatrix}). This function is not mandatory as it is carried out automatically if the user provides a distance matrix in the \code{sDistanceMatrix} field when running the \code{\link{RunResidenceExtraction}} function. 
}
\usage{
NonResidenceExtractId(sResidenceEventFile, sDistanceMatrix = NULL)
}

\arguments{
  \item{sResidenceEventFile }{a residence event table}
  \item{sDistanceMatrix }{an optional two dimentional array (matrix) containing the pairwise distances between a series of receivers}
}

\value{
  \item{STARTTIME }{a POSIXct vector object containing the date and time a transmitter left a receiver/station after a residence event}
  \item{ENDTIME }{a POSIXct vector object containing the date and time a transmitter arrived at a receiver/station and a new residence event was logged}
  \item{NONRESIDENCEEVENT }{a numeric vector indexing each nonresidence event}
  \item{TRANSMITTERID }{a numeric or character vector indexing the transmitter from which nonresidence events were determined}
  \item{RECEIVERID1 }{a numeric or character vector indexing the receiver which the transmitter initially moved from. If \code{STATIONNAME} is specified in the function, \code{STATIONNAME1} is returned}
  \item{RECEIVERID2 }{a numeric or character vector indexing the receiver which the transmitter moved to. If \code{STATIONNAME} is specified in the function, \code{STATIONNAME2} is returned}
  \item{DURATION }{a numeric vector containing the total time in seconds taken for the transmitter to move between two receivers or stations}
  \item{DISTANCE }{a numeric vector containing the minimum distance travelled in meters between two receivers/stations according to the distance matrix. If a distance matrix was not attached (\code{=NULL}), distance is returned as \code{0}}
  \item{ROM }{a numeric vector containing the rate of movement (\code{ROM}) in m/s. This is calculated from the distance travelled (i.e. \code{DISTANCE}) divided by the time taken to move between the receivers (i.e. \code{DURATION})}
}

\author{
Ross Dwyer, Mathew Watts, Hamish Campbell}

\seealso{
\code{\link{RunResidenceExtraction}}
}

\examples{
# This function runs within the RunResidenceExtraction function when 
# a distance matrix is provided by the user.

# Extract residence events at RECEIVERS from the VTrack-transformed 
# saltwater crocodile archive

# Load the crocodile data into the VTrack archive
data(crocs)  
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        dateformat = NULL,
                        sVemcoFormat='1.0')       

# Extract data for only the transmitter #139
T139 <- ExtractData(Vcrocs, 
                    sQueryTransmitterList = 139)

# Extract residence and non residence events using receiver data
# 	Minimum number of detections to register as a residence
# 	event = 2  
# 	Min time period between detections before residence event
# 	recorded = 43200 secs (12 hours)
T139Res <- RunResidenceExtraction(sInputFile=T139, 
                                 sLocation="RECEIVERID",
                                 iResidenceThreshold=1,
                                 iTimeThreshold=43200,
                                 sDistanceMatrix=NULL)

# The residences event table
T139resid <- T139Res$residences

# Generate the circuitous distance matrix
data(PointsCircuitous_crocs)
CircuitousDM <- GenerateCircuitousDistance(PointsCircuitous_crocs)

# Ensure there is only 1 Transmitter in dataset (if not, run the function within a loop)
length(unique(T139resid$TRANSMITTERID))

# Run the non-residence function
NonResidenceExtractId(sResidenceEventFile=T139resid,
                      sDistanceMatrix=CircuitousDM)
}

\keyword{behaviour}
\keyword{spatial}
