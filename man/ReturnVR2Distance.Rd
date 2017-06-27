\name{ReturnVR2Distance}
\alias{ReturnVR2Distance}
\title{
Extract the Distances Moved Between VR2 Receiver Units Within the Acoustic Detection Database
}
\description{
This function uses combines the non-residence event table with a distance matrix to extract the minimum distance moved between two receivers. This function returns a numeric vector containing the minimum distance moved between receivers (extracted from  \code{sDistanceMatrix}). This function is not mandatory as it is carried out automatically if the user provides a distance matrix in the \code{sDistanceMatrix} field when running the \code{\link{RunResidenceExtraction}} function. 
}
\usage{
ReturnVR2Distance(NonResidenceFile, sDistanceMatrix)
}

\arguments{
  \item{NonResidenceFile }{
a data frame containing the \code{nonresidences} event table
}
  \item{sDistanceMatrix }{
a two dimentional array (matrix) containing the pairwise distances between an array of VR2 receivers  
}
}
\value{
a numeric vector of minimum distance travelled (in kilometers) corresponding to the values listed in the distance matrix
}
\author{
Ross Dwyer, Mathew Watts, Hamish Campbell
}

\seealso{
\code{\link{RunResidenceExtraction}}, \code{\link{NonResidenceExtractId}}
}

\examples{
\dontrun{
# Extract residence events at RECEIVERS from the VTrack transformed 
#   crocodile dataset

# Load the crocodile dataset into the VTrack archive format
data(crocs)  
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        fAATAMS=FALSE,
                        fVemcoDualSensor=FALSE,
                        dateformat = NULL,
                        sVemcoFormat='1.0')        

# Extract data for only the transmitter #138
T138 <- ExtractData(Vcrocs, 
                    sQueryTransmitterList = 138)

# Extract residence and non residence events
#   Minimum number of detections to register as a residence
#   event = 2  
#   Min time period between detections before residence event
#   recorded = 43200 secs (12 hours)
T139Res <- RunResidenceExtraction(sInputFile=T138, 
                                 sLocation="RECEIVERID",
                                 iResidenceThreshold=2,
                                 iTimeThreshold=43200,
                                 sDistanceMatrix=NULL)

# The nonresidences event table
T139nonresid <- T139Res$nonresidences

# Generate the Direct Distance Matrix
data(PointsDirect_crocs)
DirectDM <- GenerateDirectDistance(PointsDirect_crocs)

# Run the VR2 distances function
(My_distances <- ReturnVR2Distance(NonResidenceFile = T139nonresid,
                                   sDistanceMatrix = DirectDM))
} 
}
\keyword{spatial}
\keyword{behaviour}
