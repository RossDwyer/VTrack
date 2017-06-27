\name{RunTimeProfile}
\alias{RunTimeProfile}
\title{
  Extract a Time Profile for Depth, Temperature, Residence or Non-residence Events
}
\description{
  This function groups sensor, residence or non-residence events into time profiles classified by time. By specifying the time profile as hour, day, week, or month, the respective time profile is extracted for that particular event. Users can also extract a circadian profile for each event where events are filtered for each hour in a diel cycle (24 hr) and summed across days.
}
\usage{
  RunTimeProfile(sInputFile, sDATETIMEField, sProfilePeriod)
}
\arguments{
  \item{sInputFile }{
    an event data frame containing either the residence, movement, diving or temperature events
  }
  \item{sDATETIMEField }{
    a character string identifying the \code{DATE} field (a POSIXct) used to create the time profile from the event data frame (= \code{STARTTIME}, \code{ENDTIME})
  }
  \item{sProfilePeriod }{
  a character string relating to which profile should be extracted (= \code{hour}, \code{day}, \code{week}, \code{month}, \code{circadian}) 
  }
}  

\value{
  \item{DATE }{
    a POSIXct vector object containing the date and/or time an event was initiated}
  \item{FREQ }{
    a numeric vector containing the number of events for that hour/day/month}
  \item{SENSORMAX }{
    a numeric vector containing the maximum sensor reading for the time-grouped events}
  \item{SENSORAV }{
    a numeric vector containing the mean sensor reading for the time-grouped events}
  \item{SENSORSTDEV }{
    a numeric vector containing the standard deviation for the sensor readings for the time-grouped events}
  \item{TIMESUM }{
    a numeric vector containing the total duration of the time-grouped events (seconds)}
  \item{TIMEMAX }{
    a numeric vector containing the maximum duration reading for the time-grouped events (seconds)}
  \item{TIMEAV }{
    a numeric vector containing the mean duration reading for the time-grouped events (seconds)}
  \item{TIMESTDEV }{
    a numeric vector containing the standard deviation for the duration readings for the time-grouped events (seconds)}
  \item{DETECTIONS }{
    a numeric vector containing the number of detections which form all the events recorded for that time profile}
  \item{DISTANCE }{a numeric vector containing the sum of the minimum distance travelled which form all the events recorded for that time profile}
}
\author{
  Ross Dwyer, Mathew Watts, Hamish Campbell
}

\seealso{
  \code{\link{RunResidenceExtraction}}, \code{\link{RunSensorEventExtraction}}
}
\examples{

\dontrun{
# RunTimeProfile example using residences, nonresidences and sensor events
  
# Load crocodile data and convert to a VTrack archive format 
data(crocs)
Vcrocs <- ReadInputData(infile=crocs,
                        iHoursToAdd=10,
                        fAATAMS=FALSE,
                        fVemcoDualSensor=FALSE,
                        dateformat = NULL,
                        sVemcoFormat='1.0')

# Load receiver data and generate the circuitous distance matrix 
data(PointsCircuitous_crocs)
CircuitousDM <- GenerateCircuitousDistance(PointsCircuitous_crocs)
  
# Extract depth data for transmitter #139
T139 <- ExtractData(Vcrocs,sQueryTransmitterList = c("139"))
T139_R <- ExtractUniqueValues(T139,5)
  
# Extract residence and non residence events
T139Res<- RunResidenceExtraction(T139,
                                 "RECEIVERID",
                                 2,
                                 43200,
                                 sDistanceMatrix=CircuitousDM)
# The residences event table
T139resid <- T139Res$residences
# The nonresidences event table
T139nonresid <- T139Res$nonresidences

# Generate plot dimentions
par(mfrow=c(2,2),las=1,bty="l")
  
## Plot a 
# RESIDENCES: duration/day
Vres_D <- RunTimeProfile(T139resid,"STARTTIME","day")
day_res <- tapply(Vres_D$TIMEMAX,Vres_D$DATETIME,sum)[1:25]/(60*60)
numnames <- as.Date(as.character(names(day_res)))
plot(as.vector(day_res)~numnames,pch=16,
     xlab="Day",ylab="Duration (h)",main="",ylim=c(0,23))
  
## Plot b
# MOVEMENTS: distance/month

Vmove_M <- RunTimeProfile(T139nonresid,"STARTTIME","month")
mon_mov <- tapply(Vmove_M$DISTANCE,Vmove_M$DATETIME,mean)/1000
numnames <- as.numeric(as.character(names(mon_mov)))
movdata <- rep(0,12)
movdata[numnames] <- as.vector(mon_mov)
names(movdata)<-as.character(1:12)
plot(as.vector(movdata)[9:12]~ names(movdata)[9:12],pch=16,xaxt="n",
     xlab="Month",ylab="Min distance (km)",main="")
axis(side=1,las=1, at=seq(9,12),labels=month.name[9:12])

## Plot c  
# DEPTH EVENTS: frequency/diel cycle

# Extract increasing depth sensor events for transmitter 139
T139dives <- RunSensorEventExtraction(T139,
                                      "INCREASE",
                                      "RECEIVERID",
                                      "m",
                                      0.5,
                                      (1*60*60),
                                      (60*60),
                                      0.5)
# The sensor logtable
T139divelog <- T139dives$logtable
# The sensor event file
T139diveevent <- T139dives$event
# Remove timeout events
T139diveevent<-subset(T139diveevent,T139diveevent$ENDREASON=="return")
  
Vdiv_C <- RunTimeProfile(T139diveevent,"STARTTIME","circadian")
cir_div <- tapply(Vdiv_C$FREQ,Vdiv_C$DATETIME,mean)
numnames <- as.numeric(as.character(names(cir_div)))
divdata <- rep(0,24)
divdata[numnames+1] <- as.vector(cir_div)
names(divdata)<-as.character(0:23)
plot(as.vector(divdata)~ names(divdata),pch=16,
     xlab="24 hr cycle",ylab="Number of depth events",main="")
  
}
}
\keyword{behaviour}
