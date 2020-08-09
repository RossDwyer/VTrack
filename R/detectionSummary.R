#' Calculate standard metrics of detection using passive telemetry data
#'
#'@description Produce standard metrics of detection. Metrics include Number of detections, Number of receiver stations
#'  detected on, Number of days detected, Number of Days at Liberty and Detection Index.
#'
#' @param ATTdata an 'ATT' object created using \code{\link{setupData}} containing
#'   tag detection data, metadata and station information
#' @param sub argument to define temporal subsets. Temporal subsets are currently restricted to monthly ('\%Y-\%m') or weekly ('\%Y-\%W').
#'   Defaults to monthly if none is provided.
#' @param download_date date the array was downloaded in the form 'yyyy-mm-dd' for calculating detection summary before the collection of full
#' dataset has been collected. If none provided, it is assumed complete data has been collected, and detection indices are calculated as such.
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
#' ATTdata<- setupData(Tag.Detections = IMOSdata, 
#'                     Tag.Metadata = taginfo, 
#'                     Station.Information = statinfo, 
#'                     source = "IMOS")
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
#' ## If the full dataset hasn't been collected yet, detection indicies
#' ## can be calculated to the most recent date when the acoustic array
#' ## was downloaded. Indicate latest download date in the format 'yyyy-mm-dd'
#' dSum_partial <- detectionSummary(ATTdata, download_date = "2014-01-01")
#' 
#' dSum_partial$Overall
#' 
#' dSum_partial$Subsetted
#' 
detectionSummary <- function(ATTdata, sub = '%Y-%m', download_date = NULL){

  Date.Time <-Tag.ID <-Transmitter <-Sci.Name <-Sex <-Bio <-Station.Name <-Release.Date <-Tag.Life <- Days.Detected <- NULL
  uniqueStations <- Days.at.Liberty <- start_date <- end_date <- NULL
    
  if(!inherits(ATTdata, "ATT"))
    stop("Oops! Input data needs to be an 'ATT' object.\nSet up your data first using setupData() before running this operation")
  if(!sub %in% c('%Y-%m','%Y-%W'))
    stop("Sorry! I can't recognise the temporal subset chosen.\nChoose one of the following subsets:\n\tMonthly   = '%Y-%m'\n\tWeekly  = '%Y-%W'")

  ## Combine Tag.Detection and Tag.Metadata into a combined tibble for processing
  combdata<- 
    ATTdata$Tag.Detections %>% 
    left_join(ATTdata$Tag.Metadata, by = "Transmitter") %>%
    mutate(subset = as.factor(format(Date.Time, sub)))

  ## Detection metric calculations for full tag life
  full<- 
    combdata %>%
    group_by(Tag.ID) %>%
    summarize(Transmitter = first(Transmitter),
              Sci.Name = first(Sci.Name),
              Sex = first(Sex),
              Bio = first(Bio),
              Number.of.Detections = n(),
              Number.of.Stations = n_distinct(Station.Name),
              Days.Detected = n_distinct(date(Date.Time)),
              start_date = min(first(Release.Date), min(date(Date.Time)), na.rm = T),
              end_date = 
                if(is.null(download_date)){
                  max((first(Release.Date)+first(Tag.Life)), max(date(Date.Time)), na.rm = T)
                } else {
                  min((first(Release.Date)+first(Tag.Life)), date(download_date), na.rm = T)
                },
              Days.at.Liberty = as.numeric(diff(c(start_date, end_date))),
              Detection.Index = Days.Detected / Days.at.Liberty) %>% 
    dplyr::select(-c(start_date, end_date))

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

