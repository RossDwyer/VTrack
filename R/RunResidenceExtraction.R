#' @importFrom foreach foreach

RunResidenceExtraction <- 
  function(sInputFile,sLocation,iResidenceThreshold,iTimeThreshold,sDistanceMatrix=NULL,iCores=2) 
{
  
    
  i <- NULL

  cl <- parallel::makeCluster(iCores)
  doParallel::registerDoParallel(cl)
  
  sInputFile <- unique(sInputFile[order(as.character(sInputFile$DATETIME)), 
  ])
  
  TransmitterNames <- ExtractUniqueValues(sInputFile, 2)
  iTransmitterCount <- length(TransmitterNames)
  
  if (sLocation == "STATIONNAME") 
    iLocationCol <- 6
  if (sLocation == "RECEIVERID") 
    iLocationCol <- 5
  
  NonResidenceExtractId <- function (sResidenceEventFile, sDistanceMatrix = NULL) 
  {
    if (length(unique(sResidenceEventFile$TRANSMITTERID)) > 1) 
      stop("length(TRANSMITTERID) > 1. Unable to extract non-residences.")
    if (length(unique(sResidenceEventFile[, 5])) < 2) {
      newnonresidencetable <- data.frame(STARTTIME = sResidenceEventFile[1, 
                                                                         1], ENDTIME = sResidenceEventFile[1, 2], NONRESIDENCEEVENT = 0, 
                                         TRANSMITTERID = "", RECEIVERID1 = "", RECEIVERID2 = "", 
                                         DURATION = 0, DISTANCE = 0, ROM = 0)[NULL, ]
    }
    else {
      
      RECEIVERID1 <- sResidenceEventFile[c(1:length(sResidenceEventFile[, 
                                                                        5]) - 1), 5]
      RECEIVERID2 <- sResidenceEventFile[c(2:length(sResidenceEventFile[, 
                                                                        5])), 5]
      STARTTIME <- sResidenceEventFile[c(1:length(sResidenceEventFile[, 
                                                                      2]) - 1), 2]
      ENDTIME <- sResidenceEventFile[c(2:length(sResidenceEventFile[, 
                                                                    2])), 1]
      TRANSMITTERID <- sResidenceEventFile[c(2:length(sResidenceEventFile[, 
                                                                          2])), 4]
      DURATION <- as.numeric(difftime(as.POSIXct(ENDTIME), 
                                      as.POSIXct(STARTTIME), units = "secs"))
      
      
      # modification 
      NONRESIDENCEEVENT <- 1
      nonresidencetable <- na.omit(data.frame(STARTTIME, ENDTIME, 
                                              NONRESIDENCEEVENT, TRANSMITTERID, RECEIVERID1, RECEIVERID2, 
                                              DURATION))
      
      
      if (is.null(sDistanceMatrix) != TRUE) {
        DISTANCE <- ReturnVR2Distance(nonresidencetable, 
                                      sDistanceMatrix) * 1000
        ROM <- DISTANCE/nonresidencetable$DURATION
      }
      else {
        DISTANCE <- 0
        ROM <- 0
      }
      newnonresidencetable <- data.frame(nonresidencetable, 
                                         DISTANCE, ROM)
    }
    
    if (nrow(newnonresidencetable) >= 1) 
      newnonresidencetable$NONRESIDENCEEVENT <- c(1:length(newnonresidencetable[, 
                                                                                1]))
    return(newnonresidencetable)
  }
  
  ResidenceExtractId <- function(sTransmitterId) 
  {
    iCount <- 1
    iResidenceEvents <- 0
    ilogevent <- 1
    
    event <- data.frame(STARTTIME = sInputFile[1, 1], ENDTIME = sInputFile[1, 
                                                                           1], RESIDENCEEVENT = 0, TRANSMITTERID = TransmitterNames[1], 
                        RECEIVERID = as.character(sInputFile[1, iLocationCol]), 
                        DURATION = 0, ENDREASON = NA, NUMRECS = 0, stringsAsFactors = FALSE)[NULL,] 
    
    logtable <- data.frame(DATETIME = sInputFile[1, 1], RESIDENCEEVENT = 0, 
                           RECORD = 0, TRANSMITTERID = TransmitterNames[1], 
                           RECEIVERID = as.character(sInputFile[1, iLocationCol]), 
                           ELAPSED = 0, stringsAsFactors = FALSE)[NULL, ]
    
    infile <- ExtractData(sInputFile, sQueryTransmitterList = TransmitterNames[sTransmitterId])
    
    fResidenceEventStarted <- FALSE
    sEndReason <- ""
    iRecordsProcessed <- 0
    iNumberOfRecords <- 0
    sRECEIVERID <- ""
    sPreviousReceiver <- ""
    sSTARTTIME <- ""
    sPreviousTime <- ""
    
    while (iCount <= nrow(infile)) 
    {
      
      sDATETIME <- infile[iCount, 1]
      sRECEIVERID <- as.character(infile[iCount, iLocationCol])
      
      iRecordsProcessed <- iRecordsProcessed + 1
      if (fResidenceEventStarted) 
      {
        iNumberOfRecords <- iNumberOfRecords + 1
        iElapsedTime <- as.numeric(difftime(as.POSIXct(as.character(sDATETIME)), 
                                            as.POSIXct(as.character(sPreviousTime)), units = "secs"))
        logtable[ilogevent, ] <- data.frame(DATETIME = as.character(sDATETIME), 
                                            RESIDENCEEVENT = iResidenceEvents, RECORD = iNumberOfRecords, 
                                            TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                            RECEIVERID = as.character(sRECEIVERID), ELAPSED = iElapsedTime, 
                                            stringsAsFactors = FALSE)
        ilogevent <- ilogevent + 1
        if (iElapsedTime > iTimeThreshold) 
        {
          fResidenceEventStarted <- FALSE
          sEndReason <- "timeout"
        }
        # modification
        if (iElapsedTime <= iTimeThreshold & sPreviousReceiver != sRECEIVERID) 
        {
          fResidenceEventStarted <- FALSE
          sEndReason <- "receiver"
        }
        if (iCount == nrow(infile)) {
          fResidenceEventStarted <- FALSE
          sEndReason <- "signal lost"
        }
        if (fResidenceEventStarted) 
        {
        }else {
          if (iNumberOfRecords >= iResidenceThreshold) 
          {
            iElapsedTime <- as.numeric(difftime(as.POSIXct(as.character(sPreviousTime)), 
                                                as.POSIXct(as.character(sSTARTTIME)), units = "secs"))
            event[iResidenceEvents, ] <- data.frame(STARTTIME = as.character(sSTARTTIME), 
                                                    ENDTIME = as.character(sPreviousTime), 
                                                    RESIDENCEEVENT = iResidenceEvents, 
                                                    TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                                    RECEIVERID = as.character(sPreviousReceiver), 
                                                    DURATION = iElapsedTime, ENDREASON = sEndReason, 
                                                    NUMRECS = iNumberOfRecords, stringsAsFactors = FALSE)
          }
        }
      }
      else {
        if (iRecordsProcessed > 1) {
          iElapsedTime <- as.numeric(difftime(as.POSIXct(as.character(sDATETIME)), as.POSIXct(as.character(sPreviousTime)), units = "secs"))
          if (iElapsedTime < iTimeThreshold) 
          {
            iResidenceEvents <- iResidenceEvents + 1
            iNumberOfRecords <- 1
            sSTARTTIME <- sPreviousTime
            sEndReason <- ""
            logtable[ilogevent, ] <- data.frame(DATETIME = sPreviousTime, 
                                                RESIDENCEEVENT = iResidenceEvents, RECORD = 0, 
                                                TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                                RECEIVERID = as.character(sPreviousReceiver), 
                                                ELAPSED = 0, stringsAsFactors = FALSE)
            logtable[ilogevent + 1, ] <- data.frame(DATETIME = sDATETIME, 
                                                    RESIDENCEEVENT = iResidenceEvents, RECORD = iNumberOfRecords, 
                                                    TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                                    RECEIVERID = as.character(sRECEIVERID), 
                                                    ELAPSED = iElapsedTime, stringsAsFactors = FALSE)
            ilogevent <- ilogevent + 2
            
            if (as.character(sRECEIVERID) == as.character(sPreviousReceiver)) 
            {
              fResidenceEventStarted <- TRUE
            }else{
              fResidenceEventStarted <- FALSE
            }
          }
        }
      }
      
      sPreviousTime <- sDATETIME
      sPreviousReceiver <- as.character(sRECEIVERID)
      iCount <- iCount + 1
    }
    ilist <- new.env()
    ilist <- list(na.omit(logtable), 
                  na.omit(event), 
                  NonResidenceExtractId(na.omit(event), sDistanceMatrix))
    
    return(ilist)
  }
  
  ResidenceExtractId1 <- function(sTransmitterId) 
  {
    iCount <- 1
    iResidenceEvents <- 0
    ilogevent <- 1
    
    event <- data.frame(STARTTIME = sInputFile[1, 1], ENDTIME = sInputFile[1,1], 
                        RESIDENCEEVENT = 0, TRANSMITTERID = TransmitterNames[1], 
                        RECEIVERID = as.character(sInputFile[1, iLocationCol]), 
                        DURATION = 0, ENDREASON = NA, NUMRECS = 0, stringsAsFactors = FALSE)[NULL,]
    logtable <- data.frame(DATETIME = sInputFile[1, 1], RESIDENCEEVENT = 0, 
                           RECORD = 0, TRANSMITTERID = TransmitterNames[1], 
                           RECEIVERID = as.character(sInputFile[1, iLocationCol]), 
                           ELAPSED = 0, stringsAsFactors = FALSE)[NULL, ]
    
    infile <- ExtractData(sInputFile, sQueryTransmitterList = TransmitterNames[sTransmitterId])
    
    fResidenceEventStarted <- FALSE
    sEndReason <- ""
    iRecordsProcessed <- 0
    iNumberOfRecords <- 0
    sRECEIVERID <- ""
    sPreviousReceiver <- ""
    sSTARTTIME <- ""
    sPreviousTime <- ""
    iElapsedTime <- 0
    
    while (iCount <= nrow(infile)) 
    {
      
      sDATETIME <- infile[iCount, 1]
      sRECEIVERID <- as.character(infile[iCount, iLocationCol])
      
      iRecordsProcessed <- iRecordsProcessed + 1
      
      if (iNumberOfRecords > 1) {
        if (fResidenceEventStarted == TRUE) {
          
          iElapsedTime <- as.numeric(difftime(as.POSIXct(as.character(sDATETIME)), 
                                              as.POSIXct(as.character(sPreviousTime)), 
                                              units = "secs"))
          ilogevent <- ilogevent + 1
          logtable[ilogevent, ] <- data.frame(DATETIME = as.character(sDATETIME), 
                                              RESIDENCEEVENT = iResidenceEvents, RECORD = iNumberOfRecords, 
                                              TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                              RECEIVERID = as.character(sRECEIVERID), ELAPSED = iElapsedTime, 
                                              stringsAsFactors = FALSE)
          iNumberOfRecords <- iNumberOfRecords + 1
          
          if (iElapsedTime > iTimeThreshold) 
          {
            fResidenceEventStarted <- FALSE
            sEndReason <- "timeout"
          }
          # modification
          if (iElapsedTime <= iTimeThreshold & sPreviousReceiver != sRECEIVERID) 
          {
            fResidenceEventStarted <- FALSE
            sEndReason <- "receiver"
          }
          if (iCount == nrow(infile)) {
            fResidenceEventStarted <- FALSE
            sEndReason <- "signal lost"
          }
          if (fResidenceEventStarted == FALSE) {
            if (sEndReason != "signal lost") {
              iElapsedTimeTotal <- as.numeric(difftime(as.POSIXct(as.character(sPreviousTime)), 
                                                       as.POSIXct(as.character(sSTARTTIME)), 
                                                       units = "secs"))
              event[iResidenceEvents, ] <- data.frame(STARTTIME = as.character(sSTARTTIME), 
                                                      ENDTIME = as.character(sPreviousTime), 
                                                      RESIDENCEEVENT = iResidenceEvents, TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                                      RECEIVERID = as.character(sPreviousReceiver), 
                                                      DURATION = iElapsedTimeTotal, ENDREASON = sEndReason, 
                                                      NUMRECS = iNumberOfRecords - 1, stringsAsFactors = FALSE)
              iNumberOfRecords <- 0
              ilogevent <- ilogevent + 1
            }
            else { 
              
              # Needs an ifelse statement to catch a single line at the same receiver as the last detection after 
              # period of time that extends beyond the timeout period
              
              
              if(as.numeric(difftime(as.POSIXct(as.character(sDATETIME)), 
                                     as.POSIXct(as.character(sPreviousTime)), 
                                     units = "secs")) <= iTimeThreshold){
                
                iElapsedTimeTotal <- as.numeric(difftime(as.POSIXct(as.character(sDATETIME)), # RD edit Nov 2022
                                                         as.POSIXct(as.character(sSTARTTIME)), 
                                                         units = "secs"))
                event[iResidenceEvents, ] <- data.frame(STARTTIME = as.character(sSTARTTIME), 
                                                        ENDTIME = as.character(sDATETIME),
                                                        RESIDENCEEVENT = iResidenceEvents, 
                                                        TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                                        RECEIVERID = as.character(sPreviousReceiver), 
                                                        DURATION = iElapsedTimeTotal, 
                                                        ENDREASON = sEndReason, 
                                                        NUMRECS = iNumberOfRecords, stringsAsFactors = FALSE)
              }
              else {
                
                ### iElapsedTimeTotal <- as.numeric(difftime(as.POSIXct(as.character(sDATETIME)), # RD edit Nov 2022
                iElapsedTimeTotal <- as.numeric(difftime(as.POSIXct(as.character(sPreviousTime)), # RD edit Nov 2022
                                                         as.POSIXct(as.character(sSTARTTIME)), 
                                                         units = "secs"))
                event[iResidenceEvents, ] <- data.frame(STARTTIME = as.character(sSTARTTIME), 
                                                        ###ENDTIME = as.character(sDATETIME),  # RD edit Nov 2022
                                                        ENDTIME = as.character(sPreviousTime), # RD edit Nov 2022
                                                        RESIDENCEEVENT = iResidenceEvents, 
                                                        TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                                        RECEIVERID = as.character(sPreviousReceiver), 
                                                        DURATION = iElapsedTimeTotal, 
                                                        ENDREASON = "timeout", 
                                                        NUMRECS = iNumberOfRecords, stringsAsFactors = FALSE)
                
                # Write that last line for the Nov 2022 update
                iResidenceEvents <- iResidenceEvents+1
                event[iResidenceEvents, ] <- data.frame(STARTTIME = as.character(sDATETIME), 
                                                        ENDTIME = as.character(sDATETIME), 
                                                        RESIDENCEEVENT = iResidenceEvents, 
                                                        TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                                        RECEIVERID = as.character(sRECEIVERID), 
                                                        DURATION = 0, 
                                                        ENDREASON = sEndReason, 
                                                        NUMRECS = 1, 
                                                        stringsAsFactors = FALSE)
                ilogevent <- ilogevent + 1
                logtable[ilogevent, ] <- data.frame(DATETIME = as.character(sDATETIME), 
                                                    RESIDENCEEVENT = iResidenceEvents, RECORD = 1, 
                                                    TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                                    RECEIVERID = as.character(sRECEIVERID), ELAPSED = 0, 
                                                    stringsAsFactors = FALSE)
              }
              
              
            }
          }
        }
      }
      
      if (iNumberOfRecords == 1) {
        iElapsedTime <- as.numeric(difftime(as.POSIXct(as.character(sDATETIME)), as.POSIXct(as.character(sPreviousTime)), units = "secs"))
        if (iElapsedTime >= iTimeThreshold | sPreviousReceiver != sRECEIVERID) {
          if (iElapsedTime > iTimeThreshold) 
            sEndReason <- "timeout"
          # modification
          if (iElapsedTime <= iTimeThreshold & sPreviousReceiver != sRECEIVERID) 
            sEndReason <- "receiver"
          if (iCount == nrow(infile)) 
            sEndReason <- "signal lost"
          if (sEndReason == "receiver") 
            event[iResidenceEvents, ] <- data.frame(STARTTIME = as.character(sSTARTTIME), 
                                                    ENDTIME = as.character(sPreviousTime), 
                                                    RESIDENCEEVENT = iResidenceEvents, TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                                    RECEIVERID = as.character(sPreviousReceiver), 
                                                    DURATION = 0, ENDREASON = sEndReason, NUMRECS = iNumberOfRecords, 
                                                    stringsAsFactors = FALSE)
          
          if (sEndReason != "receiver" & sEndReason != "") 
            event[iResidenceEvents, ] <- data.frame(STARTTIME = as.character(sSTARTTIME), 
                                                    ENDTIME = as.character(sPreviousTime), 
                                                    RESIDENCEEVENT = iResidenceEvents, TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                                    RECEIVERID = as.character(sPreviousReceiver), 
                                                    DURATION = iElapsedTime, ENDREASON = sEndReason, 
                                                    NUMRECS = iNumberOfRecords, stringsAsFactors = FALSE)
          iNumberOfRecords <- 0
          
          fResidenceEventStarted <- FALSE
          
          ilogevent <- ilogevent + 1
          logtable[ilogevent, ] <- data.frame(DATETIME = sDATETIME, 
                                              RESIDENCEEVENT = iResidenceEvents, RECORD = 1, 
                                              TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                              RECEIVERID = as.character(sRECEIVERID), ELAPSED = iElapsedTime, 
                                              stringsAsFactors = FALSE)
        }
        
        if (iElapsedTime < iTimeThreshold & sPreviousReceiver == sRECEIVERID) {
          iNumberOfRecords <- iNumberOfRecords + 1
          ilogevent <- ilogevent + 1
          logtable[ilogevent, ] <- data.frame(DATETIME = sDATETIME, 
                                              RESIDENCEEVENT = iResidenceEvents, RECORD = 1, 
                                              TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                              RECEIVERID = as.character(sRECEIVERID), ELAPSED = iElapsedTime, 
                                              stringsAsFactors = FALSE)
          fResidenceEventStarted <- TRUE
        }
      }
      
      if (iNumberOfRecords == 0) {
        sSTARTTIME <- sDATETIME
        iResidenceEvents <- iResidenceEvents + 1
        iNumberOfRecords <- iNumberOfRecords + 1
        logtable[ilogevent, ] <- data.frame(DATETIME = sSTARTTIME, 
                                            RESIDENCEEVENT = iResidenceEvents, RECORD = 0, 
                                            TRANSMITTERID = as.character(TransmitterNames[sTransmitterId]), 
                                            RECEIVERID = as.character(sRECEIVERID), ELAPSED = 0, 
                                            stringsAsFactors = FALSE)
      }
      
      iCount <- iCount + 1  
      sPreviousTime <- sDATETIME
      sPreviousReceiver <- as.character(sRECEIVERID)
    }
    
    
    ilist <- new.env()
    ilist <- list(na.omit(logtable), na.omit(event), NonResidenceExtractId(na.omit(event), 
                                                                           sDistanceMatrix))
    return(ilist)
  }
  
  if (iResidenceThreshold > 1) 
    results <- foreach(i = 1:iTransmitterCount, .packages = "VTrack") %dopar% ResidenceExtractId(i)
  
  if (iResidenceThreshold == 1) 
    results <- foreach(i = 1:iTransmitterCount, .packages = "VTrack") %dopar% ResidenceExtractId1(i)
  
  ilist2 <- new.env()
  
  ilist2$residences <- do.call(rbind, (do.call(rbind, results)[, 
                                                               2]))
  names(ilist2$residences)[5] <- sLocation
  ilist2$residences$DURATION <- difftime(ilist2$residences$ENDTIME,ilist2$residences$STARTTIME,units="secs")
  
  ilist2$residenceslog <- do.call(rbind, (do.call(rbind, results)[, 
                                                                  1]))
  names(ilist2$residenceslog)[5] <- sLocation
  
  ilist2$nonresidences <- do.call(rbind, (do.call(rbind, results)[, 
                                                                  3]))
  names(ilist2$nonresidences)[5] <- paste(sLocation, "1", sep = "")
  names(ilist2$nonresidences)[6] <- paste(sLocation, "2", sep = "")
  
  return(as.list(ilist2))
  
  doParallel::stopImplicitCluster()
  parallel::stopCluster(cl) # added in Nov 2022
}

}
