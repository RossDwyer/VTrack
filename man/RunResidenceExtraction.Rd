\name{RunResidenceExtraction}
\alias{RunResidenceExtraction}
\title{Extract Residence and Nonresidence Events Within the Acoustic Detection Database}
\description{
Events when the transmitter remained within the detection field of a given receiver. The event is triggered when a transmitter is detected by a receiver and terminated when the transmitter is detected at another receiver, or if the transmitter is not detected by the same receiver within a user defined timeout window. \code{nonresidences} (i.e. when a transmitter moves between the detection fields of two receivers) are generated from the \code{residences} event table. The function returns a list object containing a \code{residenceslog}, a \code{residences} event table and a \code{nonresidences} event table.
}
\usage{
RunResidenceExtraction(sInputFile, sLocation, iResidenceThreshold, iTimeThreshold, 
    sDistanceMatrix = NULL,iCores = 2)
}
\arguments{
  \item{sInputFile }{a data frame containing VTrack archive data, this archive is created using the \code{\link{ReadInputData}} function}
  \item{sLocation }{the location at which we wish to analyse our residence and non-residence events (i.e. \code{RECEIVERID} or \code{STATIONNAME})}
  \item{iResidenceThreshold }{the minimum number of successive transmitter pings detected at a receiver before a residence event is recorded}
  \item{iTimeThreshold }{the minimum time period in seconds between pings before a residence event is recorded}
  \item{sDistanceMatrix }{an optional two dimensional array containing the distances between a set of points. This can be the distances between each receiver or between each station. The first column in this matrix must contain the names of the receivers/stations. The diagonal of the distance matrix should be printed as \code{0} if appropriate and the upper triange of the distance matrix should also be calculated. Row 1 must contain the names of the corresponding receivers/stations and column 1 should be named \code{DM}. If a distance matrix is not available, Distances moved and the rate of movement (\code{ROM}) are returned as \code{0} in the \code{nonresidences} event table
}
  \item{iCores }{the the number of cores with which to run the function in parallel. Default is 2.}

}
\value{
A \code{list} object containing 3 tables. In the \code{residenceslog} table:
  \item{DATETIME }{a POSIXct vector object containing the date and time that the information was logged at the receiver}
  \item{RESIDENCEEVENT }{a numeric vector indexing all the individual detections which make up each particular residence event listed in the residence event table}
  \item{RECORD }{a numeric vector indexing each detection within the event}
  \item{TRANSMITTERID }{a numeric or character vector indexing the transmitter from which residence events were determined}
  \item{RECEIVERID }{a numeric or character vector indexing the receiver where the event occurred. If \code{STATIONNAME} is specified in the function, \code{STATIONNAME} is returned}
  \item{ELAPSED }{a numeric vector containing the total time in seconds of the event}

In the \code{residences} event table:
  \item{STARTTIME }{a POSIXct vector object containing the date and time a residence event was initiated}
  \item{ENDTIME }{a POSIXct vector object containing the date and time a residence event ended}
  \item{RESIDENCEEVENT }{a numeric vector indexing each particular event back to the residenceslog table where all the individual detections making up the event can be viewed}
  \item{TRANSMITTERID }{a numeric or character vector indexing the transmitter from which residence events were determined}
  \item{RECEIVERID }{a numeric or character vector indexing the receiver where the event occurred. If \code{STATIONNAME} is specified in the function, \code{STATIONNAME} is returned}
  \item{DURATION }{a numeric vector containing the time in seconds from the first to last detection within the event}
  \item{ENDREASON }{a character vector containing the reason why the residence event ended. This may be due to the transmitter appearing at another receiver (\code{receiver}) or if the last detection had passed the user defined timeout threshold (\code{timeout}). \code{signal lost} indicates the last recording of each transmitter.}
  \item{NUMRECS }{a numeric vector containing the number of records detected within each event}

In the \code{nonresidences} event table:
  \item{STARTTIME }{a POSIXct vector object containing the date and time a transmitter left a receiver or station}
  \item{ENDTIME }{a POSIXct vector object containing the date and time a transmitter arrived at a different receiver orstation}
  \item{NONRESIDENCEEVENT }{a numeric vector indexing each nonresidence event}
  \item{TRANSMITTERID }{a numeric or character vector indexing the transmitter from which nonresidence events were determined}
  \item{RECEIVERID1 }{a numeric or character vector indexing the receiver which the transmitter initially moved from. If \code{STATIONNAME} is specified in the function, \code{STATIONNAME1} is returned}
  \item{RECEIVERID2 }{a numeric or character vector indexing the receiver which the transmitter moved to. If \code{STATIONNAME} is specified in the function, \code{STATIONNAME2} is returned}  
  \item{DURATION }{a numeric vector containing the total ime in seconds taken for the transmitter to move between the two receivers}
  \item{DISTANCE }{a numeric vector containing the minimum distance travelled (m) between two receivers or stations according to the distance matrix. If a distance matrix was not attached (\code{NULL}), distance is returned as \code{0}}
  \item{ROM }{a numeric vector containing the rate of movement (ROM) in m/s. This is calculated from the distance travelled (\code{DISTANCE}) divided by the time taken to move between the receivers (\code{DURATION})}
}
\author{Ross Dwyer, Mathew Watts, Hamish Campbell}

\seealso{
\code{\link{ReadInputData}}, \code{\link{RunSensorEventExtraction}}, \code{\link{RunTimeProfile}}
}
\examples{

\dontrun{

# Extract residence events from the archived crocodile data

# Load the crocodile dataset into the VTrack archive format
data(crocs)
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        fAATAMS=FALSE,
                        fVemcoDualSensor=FALSE,
                        dateformat = NULL,
                        sVemcoFormat='1.0')

# Load and generate the direct distance matrix
data(PointsDirect_crocs)
DirectDM <- GenerateDirectDistance(PointsDirect_crocs)

# Extract data for only transmitter #139
T139 <- ExtractData(Vcrocs,sQueryTransmitterList = c("139"))
T139_R <- ExtractUniqueValues(T139,5)

# Extract residences and nonresidences events.
#   Events occur when >1 detection occurs at a receiver and detections
#   are less than 43200 seconds apart
#   The direct distance matrix is used for distance calculations
T139Res<- RunResidenceExtraction(T139,  
                                 "RECEIVERID",    
                                 2,              
                                 43200,
                                 sDistanceMatrix=DirectDM)

# The residenceslog table
T139log <- T139Res$residenceslog
# The residences event file
T139resid <- T139Res$residences
# The nonresidences event file
T139nonresid <- T139Res$nonresidences

# The RESIDENCEEVENT number in the residences event table corresponds
# to the RESIDENCEEVENT number in the residenceslog table
subset(T139log,T139log$RESIDENCEEVENT==2)
subset(T139resid, T139resid$RESIDENCEEVENT==2)

subset(T139log,T139log$RESIDENCEEVENT==8)
subset(T139resid, T139resid$RESIDENCEEVENT==8)

# Scale duration spent at receivers into 4 bins: <1min, <1hr, <1day, >1day  
pchDURATION <- ifelse(T139resid$DURATION<60,0.1,
                      ifelse(T139resid$DURATION<(60*60),0.5,
                             ifelse(T139resid$DURATION<(60*60*24),1,3)))

# For TRANSMITTERID 139 plot the detections against time for each RECEIVERID 
par(mfrow=c(1,1),las=1,bty="l")
plot(as.Date(T139resid$STARTTIME),
     as.numeric(as.factor(
       as.numeric(as.character(T139resid$RECEIVERID)))),
     ylab="RECEIVERID",xlab="DATETIME",
     yaxt="n",pch=1,cex.axis=0.9,cex=pchDURATION,
     main=unique(T139resid$TRANSMITTER))
axis(side=2,las=1, at=seq(1,length(T139_R),1),cex.axis=0.7,
     labels = T139_R[order(as.numeric(T139_R))])     

# Now plot the residence time at a receiver spatially and with 
# each point representing the duration spent at each receiver
myresid1 <- subset(T139resid, T139resid$ENDREASON=="receiver")
totalDur <- tapply(myresid1$DURATION,myresid1$RECEIVERID,sum)
totalDurT <- data.frame(LOCATION=names(totalDur), DURATION=as.vector(totalDur))
XYDuration <- merge(PointsDirect_crocs,totalDurT)

plot(PointsDirect_crocs$LONGITUDE,PointsDirect_crocs$LATITUDE,
     pch=1,cex=0.5,col="grey40",
     xlim=c((min(PointsDirect_crocs$LONGITUDE)-0.01),(max(PointsDirect_crocs$LONGITUDE)+0.01)),
     ylim=c((min(PointsDirect_crocs$LATITUDE)-0.01),(max(PointsDirect_crocs$LATITUDE)+0.01)),
     xlab="Longitude",ylab="Latitude",
     main=unique(T139resid$TRANSMITTER))
points(XYDuration$LONGITUDE,XYDuration$LATITUDE,
       cex=XYDuration$DURATION/500000, pch=16)
}
}
\keyword{behaviour}
\keyword{spatial}