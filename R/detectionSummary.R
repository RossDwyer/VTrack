#' Calculate standard metrics of detection using passive telemetry data
#'
#'@description Produce standard metrics of detection. Metrics include Number of detections, Number of receiver stations
#'  detected on, Number of days detected, Number of Days at Liberty and Detection Index.
#'
#' @param ATTdata an 'ATT' object created using \code{\link{setupData}} containing
#'   tag detection data, metadata and station information
#' @param sub argument to define temporal subsets. Temporal subsets are currently restricted to monthly ('\%Y-\%m') or weekly ('\%Y-\%W').
#'   Defaults to monthly if none is provided.
#'
#' @return Produces a list of tibbles containing Overall (full tag life) and Subsetted (user-defined temporal subsets) metrics of detection.
#'   Temporal subsets are currently restricted to monthly ('\%Y-\%m') or weekly ('\%Y-\%W').
#'
#' @seealso Input data needs to be setup using \code{\link{setupData}}
#' @export
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @examples
#' ## Import example datasets
#' data(IMOSdata)
#' data(taginfo)
#' data(statinfo)
#'
#' ## Setup data
#' ATTdata<- setupData(Tag.Detections = IMOSdata, Tag.Metadata = taginfo, Station.Information = statinfo, source="IMOS")
#'
#' ## Estimate detecion metrics with monthly subsets chosen
#' dSum<-detectionSummary(ATTdata, sub = "%Y-%m")
#'
#' ## Metrics of detection for full tag life
#' dSum$Overall
#'
#' ## Metrics of detection for each temporal subset
#' dSum$Subsetted
#'
detectionSummary<-function(ATTdata, sub='%Y-%m'){

  Date.Time <-Tag.ID <-Transmitter <-Sci.Name <-Sex <-Bio <-Station.Name <-Release.Date <-Tag.Life <- Days.Detected <- NULL
  uniqueStations <- Days.at.Liberty <- NULL
    
  if(!inherits(ATTdata, "ATT"))
    stop("Oops! Input data needs to be an 'ATT' object.\nSet up your data first using setupData() before running this operation")
  if(!sub %in% c('%Y-%m','%Y-%W'))
    stop("Sorry! I can't recognise the temporal subset chosen.\nChoose one of the following subsets:\n\tMonthly   = '%Y-%m'\n\tWeekly  = '%Y-%W'")

  ## Combine Tag.Detection and Tag.Metadata into a combined tibble for processing
  combdata<- left_join(ATTdata$Tag.Detections, ATTdata$Tag.Metadata, by="Transmitter") %>%
    mutate(subset = as.factor(format(Date.Time, sub)))

  ## Detection metric calculations for full tag life
  full<- combdata %>%
    group_by(Tag.ID) %>%
    summarize(Transmitter = first(Transmitter),
              Sci.Name = first(Sci.Name),
              Sex = first(Sex),
              Bio = first(Bio),
              Number.of.Detections = n(),
              Number.of.Stations = n_distinct(Station.Name),
              Days.Detected = n_distinct(date(Date.Time)),
              Days.at.Liberty = as.numeric(diff(c(
                min(first(Release.Date), min(date(Date.Time)), na.rm=T),
                max((first(Release.Date)+first(Tag.Life)), max(date(Date.Time)), na.rm=T)))),
              Detection.Index = Days.Detected / Days.at.Liberty)

  ## Detection metric calculations for temporal subsets
  tsub<- combdata %>%
    group_by(Tag.ID, subset) %>%
    summarize(Transmitter = first(Transmitter),
              Sci.Name = first(Sci.Name),
              Sex = first(Sex),
              Bio = first(Bio),
              Number.of.Detections = n(),
              Number.of.Stations = n_distinct(Station.Name),
              Days.Detected = n_distinct(date(Date.Time)),
              uniqueStations = list(unique(Station.Name))) %>%
    ungroup()

  ## Number of new stations detected on between temporal subsets
  for(i in 1:length(unique(tsub$Tag.ID))){
    subdata<-subset(tsub[c("Tag.ID","subset","uniqueStations")], Tag.ID %in% unique(Tag.ID)[i])
    if(nrow(subdata)>1){
      subdata$New.Stations<-c(NA,sapply(2:nrow(subdata), function(x) New.Stations=
                                          length(
                                            setdiff(unlist(subdata$uniqueStations[x]),
                                                    unlist(subdata$uniqueStations[x-1])))))
    }else{
      subdata$New.Stations<-NA
    }
    ifelse(i%in%1, res<-subdata, res<-rbind(res,subdata))
  }

  tsub<- tsub %>%
    dplyr::select(-uniqueStations) %>%
    left_join(res[c("Tag.ID", "subset","New.Stations")], by=c("Tag.ID", "subset"))

  ## Calculate detection index during each temporal subset
  if(sub%in%"%Y-%m"){tsub$Days.at.Liberty<-days_in_month(ymd(paste(tsub$subset,01,sep="-")))}
  if(sub%in%"%Y-%W"){tsub$Days.at.Liberty<-7}
  tsub$Detection.Index<-tsub$Days.Detected/tsub$Days.at.Liberty

  return(list(Overall = full, Subsetted = tsub))
}

