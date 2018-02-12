GenerateCircuitousDistance <-
function(sPointsFile) 
{
  DistanceDF <- ConvertPointsToCircuitousDistance(sPointsFile)
  DistanceDF$DISTANCE <- as.numeric(DistanceDF$DISTANCE)
  IDs <- unique(sPointsFile[, 1])[-which(unique(sPointsFile[, 
                                                            1]) == 0)]
  rDetectionRadius <- na.omit(sPointsFile[, 4])/1000
  nxy <- length(IDs) - 1
  d <- matrix(data = 0, nrow = length(IDs), ncol = length(IDs))
  dr <- matrix(data = 0, nrow = length(IDs), ncol = length(IDs))
  for (j in 1:nxy) {
    for (i in j:nxy) {
      d[i + 1, j] <- sum(DistanceDF[j:i, 3])
      d[j, i + 1] <- d[i + 1, j]
    }
  }
  for (j in 1:nxy + 1) {
    for (i in j:nxy) {
      if (i != j) {
        dr[i, j] <- rDetectionRadius[j] + rDetectionRadius[i]
        dr[j, i] <- dr[i, j]
      }
    }
  }
  d1 <- d - dr
  for (i in 1:length(d1)) if (d1[i] <= 0) 
    d1[i] = 0
  else d1[i] = d1[i]
  d1 <- data.frame(DM = IDs, d1)
  colnames(d1) <- c("DM", IDs)
  return(d1)
}
