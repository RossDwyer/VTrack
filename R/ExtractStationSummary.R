# New function for VTrack v 1.2
ExtractStationSummary <- 
function(sInputFile) 
{
  StationList <- ExtractUniqueValues(sInputFile, 6)
  fExtractsummaryStationid <- function(i) {
    T1 <- ExtractData(sInputFile, sQueryStationList = StationList[i])
    T1 <- T1[order(T1$DATETIME), ]
    row.names(T1) <- NULL
    STATIONNAME <- T1[1, 6]
    FIRSTDETECT <- min(T1[1, 1])
    NODETECTS <- nrow(T1)
    LASTDETECT <- T1[NODETECTS, 1]
    NOTRANSMITTER <- length(unique(T1$TRANSMITTERID))
    return(data.frame(STATIONNAME, FIRSTDETECT, NODETECTS, LASTDETECT, NOTRANSMITTER))
  }
  lnewdf <- lapply(1:length(StationList), fExtractsummaryStationid)
  newdf <- do.call(rbind, lnewdf)
  return(newdf)
}
