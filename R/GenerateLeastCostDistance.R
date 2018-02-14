GenerateLeastCostDistance <- 
function(sPointsFile,sTransition)
{
  xy <- coordinates(sPointsFile[,2:3])
  SP <- SpatialPoints(xy,proj4string=CRS(proj4string(sTransition)))
  
  # Initialise variables
  newDM <- matrix(0,nrow(xy),nrow(xy))
  iCount <- 1
  rDISTANCE <- rep(0,nrow(xy)-1)
  sDISTANCE <- rep(0,nrow(xy)-1)
  
  for(i in 1:nrow(xy))     # For each row in the distance matrix
  {
    for (j in 1:nrow(xy))  # For each column in the distance matrix
    {
      SP1 <- SP[i,]
      SP2 <- SP[j,]
      
      crowdist <- spDists(SP1,SP2, longlat = FALSE) # Extract the direct distance
      
      if(crowdist > 200){
        sPath2 <- shortestPath(sTransition, SP1, SP2, output="SpatialLines")
        riverdist <- SpatialLinesLengths(sPath2,longlat=FALSE)}
      if(crowdist <= 200)
        riverdist <- crowdist
      
      #sDISTANCE[j] <- crowdist      
      rDISTANCE[j] <- riverdist 
    }
    newDM[i,] <- rDISTANCE
    print(paste0(i,"of ",nrow(xy)," receivers calculated"))
  }
  newDM <- newDM/1000 #divide by 1000 to convert from m to km
  
  # Format and return the distance matrix
  DM <- as.character(sPointsFile$LOCATION)
  colnames(newDM) <- DM
  newDM <- data.frame(newDM)
  DM2 <- data.frame(DM = DM,newDM)
  return(DM2)
}

