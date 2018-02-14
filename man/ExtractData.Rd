\name{ExtractData}
\alias{ExtractData}
\title{
Filter a Subset of Data from a VTrack File
}
\description{
ExtractData enables the user to extract/remove a subset of data (i.e. transmitters, receivers, stations and time period) from the file. For dual sensor data, this function also allows the user to extract sensor only data (i.e. temperature or depth data) from the file. 
}
\usage{
ExtractData(sInputFile, sQuerySTARTTIME = NULL, sQueryENDTIME = NULL, 
  sQueryTransmitterList = NULL, sQueryReceiverList = NULL, 
  sQueryStationList = NULL, sQueryDataType = NULL)
}
\arguments{
  \item{sInputFile }{
a data frame containing VTrack-transformed acoustic tracking data
}
  \item{sQuerySTARTTIME }{
an optional POSIXct string specifying the date/time start point from which data will be extracted from the original file. Date and time must be in the format \code{'yyyy-mm-dd HH:MM:SS'}. Default is \code{NULL}
}
  \item{sQueryENDTIME }{
an optional POSIXct string specifying the date/time end point from which data will not be extracted from the original file. Date and time must be in the format \code{'yyyy-mm-dd HH:MM:SS'}. Default is \code{NULL}
}
  \item{sQueryTransmitterList }{
an optional character string specifying the individual transmitters to be extracted from the original file. Default is \code{NULL}
}
  \item{sQueryReceiverList }{
an optional character string specifying the receivers to be extracted from the original file. Default is \code{NULL}
}
  \item{sQueryStationList }{
an optional character string specifying the stations to be extracted from the original file. Default is \code{NULL}
}
  \item{sQueryDataType }{
an optional character string specifying the sensor data type (e.g. depth \code{m}) to be extracted from the original file. Default is \code{NULL}
}
}

\value{
Subsets the original a data frame returning the following components:
  \item{DATETIME }{a vector of class POSIXct of the time of location fix of type \code{'yyyy-mm-dd HH:MM:SS'}}
  \item{TRANSMITTERID }{a numeric vector giving the identity of each transmitter (= \code{ID})}
  \item{SENSOR1 }{a numeric vector containing the value of the environmental sensor (i.e. temperature or depth) at the time of detection}
  \item{UNITS1 }{a character vector containing the units of each sensor value (e.g. \code{m}) }
  \item{TRANSMITTERID }{a character vector containing the identity of each transmitter (= \code{ID or tag ID})}
  \item{RECEIVERID }{a character vector containing the factory assigned receiver serial number (= \code{Receiver S/N or receiver ID})}
  \item{STATIONNAME }{a character vector containing the user defined station name (= \code{Station.Name or station name})}
}

\author{
Ross Dwyer, Mathew Watts, Hamish Campbell
}

\seealso{
\code{\link{ExtractUniqueValues}}
}
\examples{
# Load the crocodile data set
data(crocs)

# Convert data into the VTrack archive format
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        dateformat = NULL,
                        sVemcoFormat='1.0')

# Extract list of transmitters from test archive 1
(TransmitterList <- ExtractUniqueValues(Vcrocs,2))  


# Extract data from an archive only for the receivers listed.
R103555 <- ExtractData(Vcrocs,
                  sQueryReceiverList = 10355)

# Extract depth only data for a certain time period.
Vcrocs_Depth <- ExtractData(Vcrocs,
	sQueryDataType = "m",
	sQuerySTARTTIME = "2008-08-01 21:00:00",
	sQueryENDTIME = "2009-10-31 23:03:00")  

# Plot the detections against time for each TRANSMITTERID
par(mfrow=c(1,1),las=1,bty="l")
plot(as.Date(Vcrocs$DATETIME), as.numeric(as.factor(as.numeric(as.character(
	Vcrocs$TRANSMITTERID)))),
     ylab="TRANSMITTERID",xlab="DATETIME",
     yaxt="n",pch=16,cex=0.7)
axis(side=2, at=seq(1,length(TransmitterList),1),
     labels = TransmitterList[order(as.numeric(
	TransmitterList))])

# For TRANSMITTERID 139 plot the detections against time for each RECEIVERID 
par(mfrow=c(1,1),las=1,bty="l")
T139 <- ExtractData(Vcrocs,sQueryTransmitterList = c("139"))
T139_R <- ExtractUniqueValues(T139,5)
plot(as.Date(T139$DATETIME),
	as.numeric(as.factor(
	as.numeric(as.character(T139$RECEIVERID)))),
	ylab="RECEIVERID",xlab="DATETIME",
	yaxt="n",pch=16,cex.axis=0.9,cex=0.7,
	main=unique(T139[,2]))
axis(side=2,las=1, at=seq(1,length(T139_R),1),cex.axis=0.7,
     labels = T139_R[order(as.numeric(T139_R))])

# Extract data from TRANSMITTERID 139 and plot raw sensor data
par(mfrow=c(1,1),las=1,bty="l")
plot(T139$SENSOR1~
	T139$DATETIME,xlab="Date",
	ylab="Depth (m)",main=unique(T139[,2]),
	pch=16,cex=0.7)
}

\keyword{archive}
\keyword{extract}
