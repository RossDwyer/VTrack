#' Calculate metrics of dispersal using passive telemetry data
#'
#'@description Produce standard metrics of dispersal. Metrics include dispersal distance and bearings between
#'  tag release location and each detection as well as between consecutive detections.
#'
#' @param ATTdata an 'ATT' object created using \code{\link{setupData}} containing
#'   tag detection data, metadata and station information
#'
#' @return Produces a tibble containing dispersal distance and bearings between tag release location and
#'   each detection as well as between consecutive detections.
#'
#' @seealso Input data needs to be setup using \code{\link{setupData}}
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr select
#' @importFrom lubridate date
#' @importFrom lubridate ymd-hms
#' @import sp
#' @importFrom raster projection
#' @importFrom raster spTransform
#' @import maptools
#' @examples
#' ## Import example datasets
#' data(IMOSdata)
#' data(taginfo)
#' data(statinfo)
#'
#' ## Setup data
#' ATTdata<- setupData(Tag.Detections = IMOSdata, Tag.Metadata = taginfo, Station.Information = statinfo, source = "IMOS")
#'
#' ## Estimate detecion metrics with monthly subsets chosen
#' dispSum<-dispersalSummary(ATTdata)
#'
dispersalSummary<-function(ATTdata){

  Tag.ID <- "." <- NULL
  
  if(!inherits(ATTdata, "ATT"))
    stop("Oops! Input data needs to be an 'ATT' object.\nSet up your data first using setupData() before running this operation")
  
  ## Combine Tag.Detection and Tag.Metadata into a combined tibble for processing
  data <- left_join(ATTdata$Tag.Detections, ATTdata$Tag.Metadata, by="Transmitter")

  ## CRS for geographic coordinates from ATTdata object
  ll <- attr(ATTdata, "CRS")

  dispfun <- function(dat){
    if (nrow(dat) > 3) {
      if (!is.na(dat$Release.Latitude[1])) {
        pts <-
          data.frame(lat = dat$Latitude, lon = dat$Longitude)
        coordinates(pts) <-  ~ lon + lat
        projection(pts) <- ll
        pt <- data.frame(lat = dat$Release.Latitude[1],
                         lon = dat$Release.Longitude[1])
        coordinates(pt) <-  ~ lon + lat
        projection(pt) <- ll
        
        disp <-
          dat %>%
          ### Straight line distance between release location and each detection
          mutate(Release.Dispersal = spDistsN1(
            pts = pts,
            pt = pt,
            longlat = TRUE
          ))
        disp$Release.Bearing <- sapply(1:nrow(disp),
                                       function(x)
                                         gzAzimuth(from = matrix(
                                           c(disp$Release.Longitude[x], disp$Release.Latitude[x]), 1, 2
                                         ),
                                         to = matrix(
                                           c(disp$Longitude[x], disp$Latitude[x]), 1, 2
                                         )))
        disp$Release.Bearing <-
          ifelse(
            !is.na(disp$Release.Bearing) &
              disp$Release.Bearing < 0,
            disp$Release.Bearing + 360,
            disp$Release.Bearing
          )
        
        ### Straight line distance between consecutive detections
        disp$Consecutive.Dispersal <-
          c(
            spDistsN1(matrix(
              c(disp$Release.Longitude[1], disp$Release.Latitude[1]), 1, 2
            ),
            matrix(
              c(disp$Longitude[1], disp$Latitude[1]), 1, 2
            ), longlat = TRUE),
            sapply(2:nrow(disp),
                   function(x)
                     spDistsN1(matrix(
                       c(disp$Longitude[x - 1], disp$Latitude[x - 1]), 1, 2
                     ),
                     matrix(
                       c(disp$Longitude[x], disp$Latitude[x]), 1, 2
                     ), longlat = TRUE))
          )
        disp$Consecutive.Bearing <-
          c(gzAzimuth(from = matrix(
            c(disp$Release.Longitude[1], disp$Release.Latitude[1]), 1, 2
          ),
          to = matrix(
            c(disp$Longitude[1], disp$Latitude[1]), 1, 2
          )),
          sapply(2:nrow(disp),
                 function(x)
                   gzAzimuth(from = matrix(
                     c(disp$Longitude[x - 1], disp$Latitude[x - 1]), 1, 2
                   ),
                   to = matrix(
                     c(disp$Longitude[x], disp$Latitude[x]), 1, 2
                   ))))
        disp$Consecutive.Bearing <-
          ifelse(
            !is.na(disp$Consecutive.Bearing) &
              disp$Consecutive.Bearing < 0,
            disp$Consecutive.Bearing + 360,
            disp$Consecutive.Bearing
          )
        disp$Time.Since.Last.Detection <-
          c(NA, sapply(2:nrow(disp), function(x)
            difftime(disp$Date.Time[x], disp$Date.Time[x - 1], units = "secs")))
      }
      else{
        pts<-data.frame(lat=dat$Latitude, lon=dat$Longitude)
        coordinates(pts) <- ~lon+lat
        projection(pts) <- ll

        disp<-
          dat%>%
          ### Straight line distance between release location and each detection
          mutate(Release.Dispersal = NA,
                 Release.Bearing = NA)

        ### Straight line distance between consecutive detections
        disp$Consecutive.Dispersal<- c(NA, sapply(2:nrow(disp),
                                                  function(x)
                                                    spDistsN1(matrix(c(disp$Longitude[x-1],disp$Latitude[x-1]),1,2),
                                                              matrix(c(disp$Longitude[x],disp$Latitude[x]),1,2), longlat=TRUE)))
        disp$Consecutive.Bearing<- c(NA, sapply(2:nrow(disp),
                                                function(x)
                                                  gzAzimuth(from=matrix(c(disp$Longitude[x-1],disp$Latitude[x-1]),1,2),
                                                            to=matrix(c(disp$Longitude[x],disp$Latitude[x]),1,2))))
        disp$Consecutive.Bearing<- ifelse(!is.na(disp$Consecutive.Bearing) & disp$Consecutive.Bearing<0, disp$Consecutive.Bearing+360, disp$Consecutive.Bearing)
        disp$Time.Since.Last.Detection<-c(NA, sapply(2:nrow(disp), function(x) difftime(disp$Date.Time[x], disp$Date.Time[x-1], units = "secs")))
      }
    }
    else{
      disp <-
        dat %>%
        mutate(
          Release.Dispersal = NA,
          Release.Bearing = NA,
          Consecutive.Dispersal = NA,
          Consecutive.Bearing = NA,
          Time.Since.Last.Detection = NA
        )
    }
    return(disp)
  }


  disptab<-
    data %>%
    group_by(Tag.ID) %>%
    do(dispfun(.)) %>%
    ungroup()

  return(disptab)
}
