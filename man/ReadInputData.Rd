\name{ReadInputData}
\alias{ReadInputData}
\title{Read in a Raw VEMCO or AATAMS Data File into a VTrack Archive}
\description{ReadInputData extracts single or dual sensor data from a raw VEMCO or IMOS-ATF exported file to a VTrack structured data frame.}
\usage{
ReadInputData(infile, iHoursToAdd=0, fIMOSATF=FALSE, fVemcoDualSensor=FALSE, 
    dateformat = NULL, sVemcoFormat='Default')
}
\arguments{
  \item{infile }{a data frame containing VEMCO/AATAMS tracking data} 
  \item{dateformat }{an optional string containing the format of the \code{Date.Time} field}
  \item{iHoursToAdd }{the number of hours to add/subtract to convert the time-zone from Greenwich Mean Time (GMT)}
  \item{fIMOSATF }{logical. If data frame is in IMOS-ATF exported format (\code{fIMOSATF = TRUE}), \code{fVemcoDualSensor} and \code{sVemcoFormat} are ignored}
  \item{fVemcoDualSensor }{ logical. If VEMCO file contains single sensor data (\code{FALSE}), if dual sensor data (\code{TRUE})}
  \item{sVemcoFormat }{an optional string containing the format of the VEMCO file. The infile was exported from VUE in either old Version (\code{'1.0'}) format, or in the new (\code{'Default'}) format}
}
\value{
  \item{DATETIME }{a vector of class POSIXct of the time of location fix of type}
  \item{TRANSMITTERID }{a numeric vector giving the identity of each transmitter (\code{ID})}
  \item{SENSOR1 }{a numeric vector containing the value of the environmental sensor (i.e. temperature or depth) at the time of detection}
  \item{UNITS1 }{a character vector containing the units of each sensor value (e.g. \code{m}}
  \item{TRANSMITTERID }{a character vector containing the factory assigned transmitter tag id}
  \item{RECEIVERID }{a character vector containing the factory assigned receiver serial number (\code{Receiver S/N})}
  \item{STATIONNAME }{a character vector containing the user defined station name}
}
\author{Ross Dwyer, Mathew Watts, Hamish Campbell}

\examples{
# Load the crocodile dataset
data(crocs)  

# Convert data into the VTrack archive format 
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        fIMOSATF=FALSE,
                        fVemcoDualSensor=FALSE,
                        dateformat=NULL,
                        sVemcoFormat='1.0')

# Plot a frequency histogram of total detection per transmitter
NoDetect_ID <- tapply(rep(1,nrow(Vcrocs)),
                      Vcrocs$TRANSMITTERID,sum)
par(mfrow=c(1,1),las=1,bty="l")  
bp <- barplot(height=NoDetect_ID,
              ylab="Number of detections",xlab="Transmitter ID",
              axes=FALSE,axisnames=FALSE)        
labels <- names(NoDetect_ID)
text(bp, par("usr")[3],labels=labels, 
     srt=45,adj=c(1.1,1.1),xpd=TRUE,cex=0.8)
axis(2)
  
# Plot a frequency histogram of total detection per receiver
NoDetect_REC <- tapply(rep(1,nrow(Vcrocs)),Vcrocs$RECEIVERID,sum) 
bp <- barplot(height=NoDetect_REC,
              ylab="Number of detections",xlab="Receiver ID",
              axes=FALSE, axisnames=FALSE)
labels <- names(NoDetect_REC)
text(bp, par("usr")[3], labels=labels, 
     srt = 45, adj=c(1.1,1.1),xpd=TRUE,cex=0.8)
axis(2)
  
# Plot a frequency histogram of total detections over time
NoDetect_DAY <- tapply(rep(1,nrow(Vcrocs)),
                       as.Date(Vcrocs$DATETIME),sum) 
barplot(height=NoDetect_DAY,
        names.arg=names(NoDetect_DAY),
        ylab="Number of detections",
        xlab="Date")
}

\keyword{archive}