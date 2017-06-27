ExtractTagSummary <- 
  function (sInputFile,sLocation="RECEIVERID") 
{
  TransmitterList <- ExtractUniqueValues(sInputFile, 2)
  fExtractsummaryid <- function(i) {
    T1 <- ExtractData(sInputFile, sQueryTransmitterList = TransmitterList[i])
    row.names(T1) <- NULL
    TRANSMITTERID <- T1[1, 2]
    STARTDATE <- T1[1, 1]
    STARTLOC <- T1[1, sLocation]
    NODETECTS <- nrow(T1)
    ENDDATE <- T1[NODETECTS, 1]
    ENDLOC <- T1[NODETECTS, sLocation]
    NOLOCS <- length(unique(T1[,sLocation]))
    return(data.frame(TRANSMITTERID, STARTDATE, ENDDATE, 
                      NODETECTS, STARTLOC, ENDLOC, NOLOCS))
  }
  lnewdf <- lapply(1:length(TransmitterList), fExtractsummaryid)
  newdf <- do.call(rbind, lnewdf)
  return(newdf)
}
