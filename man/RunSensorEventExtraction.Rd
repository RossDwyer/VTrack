\name{RunSensorEventExtraction}
\alias{RunSensorEventExtraction}
\title{
Extract Sensor Events within an Acoustic Detection Database
}
\description{
This function identifies, qualifies and quantifies increasing or decreasing sensor events within the acoustic detection database. Events are defined by the user and are based on sensor threshold and time-out parameters between detections. These are established from changes in sensor values between detections, over a user-defined period of time. The location of the event is determined by either the station or the receiver or location
}
\usage{
RunSensorEventExtraction(sInputFile, iEventType, sLocation, iSensor, 
    rTriggerThreshold, iTimeThresholdStart, iTimeThreshold, rTerminationThreshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sInputFile }{
a dataframe containing VTrack-transformed acoustic tracking data
}
  \item{iEventType }{
the type of event the user wants to extract. This can be either an event whereby the sensor values increase within a certain time period (= \code{"INCREASE"}) or an event whereby the sensor values decrease within a certain time period (= \code{"DECREASE"})
}
  \item{sLocation }{
the location at which we wish to analyse our sensor events (i.e. \code{RECEIVERID} or \code{STATIONNAME})
}
  \item{iSensor }{
the sensor data type to be extracted from the original file. This corresponds to the sensor units (\code{UNITS1}) contained within the \code{sInputFile} data frame (e.g. Depth = \code{m})
}
  \item{rTriggerThreshold }{
the minimum change in sensor units for an event to commence
}
  \item{iTimeThresholdStart }{
the maximum time period (seconds) in which the \code{rTriggerThreshold} is reached before a sensor event commences
}
  \item{iTimeThreshold }{
the maximum time period (seconds) between detections before the sensor event is completed and the counter is reset 
}
  \item{rTerminationThreshold }{
how close the sensor must be to the starting value before a sensor event is completed and the counter is reset
}
}

\value{
A \code{list} object 2 tables. In the sensor event \code{logtable}:
  \item{DATETIME }{a vector of type POSIXct in Co-ordinated Universal Time (UTC)/ Greenwich Mean Time. The date and time that the location and sensor data was logged at the receiver}
  \item{SENSOREVENT }{a numeric vector indexing all the individual detections which make up each particular sensor event listed in the \code{event} table}
  \item{RECORD }{a numeric vector indexing each detection within the event}
  \item{TRANSMITTERID }{a numeric or character vector indexing the transmitter from which sensor events were determined}
  \item{RECEIVERID }{a numeric or character vector indexing the location where the event occurred. If \code{STATIONNAME} is specified in the function, the \code{STATIONNAME} where the event occurred is returned here}
  \item{SENSOR1 }{a numeric vector containing the duration of the event in seconds}
  \item{ELAPSED }{a numeric vector containing the total time in seconds of the event}

In the sensor \code{event} table:
  \item{STARTTIME }{a POSIXct vector object containing the date and time a sensor event was initiated}
  \item{ENDTIME }{a POSIXct vector object containing the date and time a sensor event ended}
  \item{SENSOREVENT }{a numeric vector indexing each particular event back to the \code{logtable}, where all the individual detections making up the event can be viewed}
  \item{TRANSMITTERID }{a numeric vector indexing the transmitter from which sonsor events were determined}
  \item{RECEIVERID }{a numeric vector indexing the location where the event occurred. If \code{STATIONNAME} is specified in the function, the \code{STATIONNAME} where the event occurred is returned here}
  \item{DURATION }{a numeric vector containing the duration of the event in seconds}
  \item{STARTSENSOR }{a numeric vector containing the sensor value when the event was initialised}
  \item{ENDSENSOR }{a numeric vector containing the sensor value when the event was either completed or terminated}
  \item{MAXSENSOR }{a numeric vector containing the maximum sensor value attained during the event}
  \item{ENDREASON }{a character vector providing information on why the event was terminated. If the sensor returned to a value within the termination threshold from the \code{STARTSENSOR} value and within the time threshold (\code{= return}) or exceeded the timeout threshold between successive detections (\code{= timeout})}
  \item{NUMRECS }{a number vector containing number of detections that compose the event}
}

\author{
Ross Dwyer, Mathew Watts, Hamish Campbell
}

\seealso{
\code{\link{RunResidenceExtraction}}, \code{\link{RunTimeProfile}}

}
\examples{
\dontrun{

## Example 1

# Extract depth events from transmitters attached
#   to crocodiles and plot a single diving event

# Load crocodile data
data(crocs)  
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        dateformat = NULL,
                        sVemcoFormat='1.0')          

# Extract depth data for only the transmitter #139
T139 <- ExtractData(Vcrocs, 
                    sQueryTransmitterList = 139)

# Extract increasing depth sensor events
#   Start depth event when there is an depth increase of 0.5m within 1 hr 
#   Max interval between detections = 1 hr 
#   Complete event when sensor returns within 0.5 of the starting value
T139dives <- RunSensorEventExtraction(T139,
                                    "INCREASE", 
                                    "RECEIVERID",
                                    "m", 
                                    0.5,          
                                    (1*60*60),    
                                    (60*60),      
                                    0.5)          

# The sensor logfile
T139divelog <- T139dives$logtable
# The sensor event file
T139diveevent <- T139dives$event

# Return list of event numbers where sensor events were complete
T139diveevent[which(T139diveevent$ENDREASON=="return"),"SENSOREVENT"]

# Now extract and plot a single sensor event (we have swapped the axes round 
#   to show the diving behaviour)
mylog <- subset(T139divelog,T139divelog$SENSOREVENT==19)
par(mfrow=c(1,1),las=1,bty="l")
plot(mylog$DATETIME,(mylog$SENSOR1),
     xlab="Event duration (mins)",ylab="Depth (m)",type="b",
     yaxs = "i", xaxs = "i", ylim = rev(c(0,max(mylog$SENSOR1+0.5))),
     xlim = (range(mylog$DATETIME)+(c(-60,30))),
     pch=as.character(mylog$RECORD))
title(main=paste("Id=",mylog[1,4],", event=",mylog[1,2], sep=" "))

######################################################

}
}
\keyword{behaviour}
