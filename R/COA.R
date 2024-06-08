#' Short-term Centers of Activity
#'
#'@description Function to calculate Short-term centers of activity positions from passive telemetry data.
#'  Based on technique described in: Simpfendorfer, C. A., M. R. Heupel, and R. E. Hueter. 2002.
#'  Estimation of short-term centers of activity from an array of omnidirectional hydrophones and its
#'  use in studying animal movements. Canadian Journal of Fisheries and Aquatic Sciences 59:23-32.
#'
#' @param ATTdata an 'ATT' object created using \code{\link{setupData}} containing
#'   tag detection data, metadata and station information
#' @param timestep an integer containing the temporal bin size of center of activity calculations (in minutes), default 60 min
#' @param split a conditional argument to split COA estimates by Tag.ID. Default is FALSE, which produces
#'   a single tibble with COAs from all individuals compiled. If TRUE, a list of tibbles produced with each Tag.ID seperated.
#'
#' @return Exports a 'COA' object that is a tibble (if split = FALSE) or a list of tibbles (if split = TRUE)
#'
#' @seealso Input data needs to be setup using \code{\link{setupData}}. 'COA' object required for \code{\link{HRSummary}}
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom lubridate as_datetime
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
#'                     source="IMOS")
#'
#' ## Estimate Short-term Center of Activities for all individuals
#' COAdata<- COA(ATTdata)
#'
#'
#' @export

COA <- function(ATTdata,
                timestep = 60,
                split = FALSE) {
  
  
  if(!inherits(ATTdata, "ATT"))
    stop("Oops! Input data needs to be an 'ATT' object.\nSet up your data first using setupData() before running this operation")

  
  ## Combine Tag.Detection and Tag.Metadata into a combined tibble for processing
  data <- 
    ATTdata$Tag.Detections %>% 
    left_join(ATTdata$Tag.Metadata, by = "Transmitter") %>%
    mutate(Tag.ID = factor(Tag.ID))

  step_sec <- timestep * 60
  ex <- seq(from = trunc(min(data$Date.Time, na.rm = TRUE), "day"),
            to = trunc(max(data$Date.Time, na.rm = TRUE), "day") + 86400,
            by = step_sec)
  data$TimeStep.coa <- cut(data$Date.Time, breaks = ex)
  

  cenac <-
    data %>%
    group_by(Tag.ID, TimeStep.coa, Sensor.Unit, Sci.Name, Common.Name,
             Tag.Project, Release.Latitude, Release.Longitude, Release.Date,
             Tag.Life, Tag.Status, Sex, Bio) %>%
    summarise(Latitude.coa = mean(Latitude, na.rm = TRUE),
              Longitude.coa = mean(Longitude, na.rm = TRUE),
              Sensor.Value.coa = mean(Sensor.Value),
              Number.of.Stations = n_distinct(Station.Name),
              Number.of.Detections = n(), 
              .groups = "keep") %>%
    mutate(TimeStep.coa = lubridate::as_datetime(TimeStep.coa))

  if(length(group_size(cenac)) > 1 & split == TRUE){
    cenac <- split(cenac, cenac$Tag.ID)
    attr(cenac, "class") <- c("list","COA", "ATT")
    attr(cenac, "CRS") <- attr(ATTdata, "CRS")
  }else{
    attr(cenac, "class") <- c("grouped_df","COA","ATT","tbl_df","tbl","data.frame")
    attr(cenac, "CRS") <- attr(ATTdata, "CRS")
  }
  return(cenac)
}

