COA <- function(tagdata, id, timestep, ...){

  ## Format date time
  data <- as.data.frame(tagdata)
  data$dt <- ymd_hms(data[,grep("Date",colnames(data))])
  data[,id] <- droplevels(as.factor(data[,id]))
  
  ## Convert timestep from minutes to seconds
  step_sec <- timestep*60 
  
  ## Setup temporal bins based on timesteps
  ex <- seq(from=trunc(min(data$dt, na.rm=TRUE), "day"), 
            to=trunc(max(data$dt, na.rm=TRUE), "day")+86400, 
            by=step_sec)
  data$DateTime <- cut(data$dt, breaks=ex)
  
  ## Calculate short term center of activity positions (3D if depth data available) 
  cenac <- ddply(data, c("DateTime",id), summarize,
                 Transmitter=Transmitter[1], Transmitter.Name=Transmitter.Name[1],
                 Transmitter.Serial=Transmitter.Serial[1], 
                 Sensor.Value.coa=mean(Sensor.Value),
                 Sensor.Unit=Sensor.Unit[1],
                 Latitude.coa=mean(Latitude, na.rm=T), 
                 Longitude.coa=mean(Longitude, na.rm=T), ...)
  cenac <- cenac[!is.na(cenac$Latitude.coa),]
  if(length(levels(cenac[,id])) > 1){
    cenac <- dlply(cenac, id)
  }
  return(cenac)
}
