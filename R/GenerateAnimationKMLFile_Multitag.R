GenerateAnimationKMLFile_Multitag <- 
function (sInputFile,sPointsFile,sOutputFile,sLocation="RECEIVERID") 
{
  
  COUNT <- LOCATION <- CRS <- NULL
  
  sInputFile$Date <- as.Date(sInputFile$DATETIME)
  
  Vfish2 <- sInputFile[, c("TRANSMITTERID", sLocation, "Date")]
  names(Vfish2)[2] <- 'LOCATION'
  Vfish3 <- Vfish2[!duplicated(Vfish2), ]
  Vfish4 <- with(Vfish3, table(LOCATION, Date))
  Vfish5 <- lapply(1:ncol(Vfish4), function(x) data.frame(LOCATION = row.names(Vfish4), 
                                                          COUNT = Vfish4[, x], DATE = colnames(Vfish4)[x]))
  Vfish6 <- do.call(rbind, Vfish5)
  Vfish7 <- merge(Vfish6, sPointsFile, by = "LOCATION")
  Vfish8 <- Vfish7[order(Vfish7$DATE, Vfish7$LOCATION), ]
  Vfish8$DATE <- as.POSIXct(Vfish8$DATE, format = "%Y-%m-%d")
  spp <- SpatialPoints(Vfish8[, c("LONGITUDE", "LATITUDE")], 
                       proj4string = CRS("+proj=longlat +datum=WGS84"))
  Multitag <- STIDF(spp, time = Vfish8$DATE, data = Vfish8[, 
                                                           c("LOCATION", "COUNT")])
  kml(Multitag, file = sOutputFile, shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", 
      colour = COUNT, size = COUNT, alpha = 0.75, labels = LOCATION)
}
