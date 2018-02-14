\name{GenerateAnimationKMLFile}
\alias{GenerateAnimationKMLFile}
\title{
Create Animation of Transmitter Residences and Movements to View in Google Earth
}
\description{
This function creates a Keyhole Markup Language (KML) animation of horizontal movements that can be displayed in Google Earth. The animation shows when a transmitter was within the detection field of a receiver and when it moved between receivers or stations. Users can adjust the time slider to visualise individual time periods for display. 
}
\usage{
GenerateAnimationKMLFile(sInputResidenceFile, sInputNonResidenceFile, 
    sInputPointsFile, sOutputFile, sReceiverColour)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sInputResidenceFile }{
the location of a \code{residences} event file (.csv) containing the \code{residences} table created using the \code{\link{RunResidenceExtraction}} function
}
  \item{sInputNonResidenceFile }{
the location of a \code{nonresidences} event file (.csv) containing the \code{nonresidences} table created using the \code{\link{RunResidenceExtraction}} function
}
  \item{sInputPointsFile }{
the location of a points file (.csv) containing the latitude and longitude of all the \code{RECEIVERID} or \code{STATIONNAME} locations within the array. Location data should be uploaded in decimal degrees in the WGS 84 datum. In many arrays, animals may not be capable of moving in a direct line between receivers (e.g. in river systems). VTrack offers users the flexablity to include other points (with their corresponding geographical locations) to link receivers along a circuitous path
}
  \item{sOutputFile }{
a string detailing the location and name of the output file to be created
}
  \item{sReceiverColour }{
colour of the receivers in the output .kml
}
}
\details{
the output is a .kml that can be viewed as an animation in Google Earth
}

\seealso{
\code{\link{GenerateDirectDistance}}, \code{\link{GenerateCircuitousDistance}}, \code{\link{RunResidenceExtraction}}
}

\author{
Ross Dwyer, Matthew Watts, Hamish Campbell
}
\examples{

\dontrun{
###GenerateAnimationKMLFile example

# Note, users must download Google Earth in order to visualise the kml.
# Extract residence and nonresidence events from the archived crocodile data

# Load crocodile datset into VTrack archive
data(crocs)
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        dateformat = NULL,
                        sVemcoFormat='1.0')

# Load Wenlock points file and generate circuitous distance matrix
data(PointsCircuitous_crocs)
CircuitousDM <- GenerateCircuitousDistance(PointsCircuitous_crocs)

# Extract transmitter #139 data from crocs dataset
T139 <- ExtractData(Vcrocs,sQueryTransmitterList = c("139"))

# Extract residence and nonresidence events from the archived crocodile data
#   Events occur when >1 detections occurs at a receiver and 
#   detections are less than 43200 seconds (12hrs) apart
#   The circuitous distance matrix is used for distance calculations
T139Res<- RunResidenceExtraction(T139,  
                                 "RECEIVERID",    
                                 2,              
                                 43200,
                                 sDistanceMatrix=CircuitousDM)

# The residences event file
T139resid <- T139Res$residences
# The nonresidences event file
T139nonresid <- T139Res$nonresidences

# Set working directory (in this case a temporary directory)
setwd(tempdir())

# Write the files to the temporary directory
write.csv(T139resid,"T139_resid.csv",row.names=FALSE) 
write.csv(T139nonresid,"T139_nonresid.csv",row.names=FALSE) 
write.csv(PointsCircuitous_crocs,"PointsCircuitous_crocs.csv",row.names=FALSE) 

# Now generate the .kml animation and save to the temporary directory
GenerateAnimationKMLFile("T139_resid.csv","T139_nonresid.csv","PointsCircuitous_crocs.csv",
                   "T139.KML","ff0000ff")

# This file can be found within the tempdir() directory on your computer. 
# Double-click on the .kml file to open in Google Earth
}
}
\keyword{behaviour}
\keyword{spatial}