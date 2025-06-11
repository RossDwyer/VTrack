#' Calculate metrics of dispersal using passive telemetry data
#'
#' @description Produce standard metrics of dispersal. Metrics include dispersal distance and bearings between
#'  tag release location and each detection as well as between consecutive detections.
#'
#' @param ATTdata an 'ATT' object created using \code{\link{setupData}} containing
#'   tag detection data, metadata and station information
#'
#' @return Produces a tibble containing dispersal distance and bearings between tag release location and
#'   each detection as well as between consecutive detections.
#'
#' @seealso Input data needs to be setup using \code{\link{setupData}}
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr reframe
#' @importFrom dplyr select
#' @importFrom dplyr lag
#' @importFrom dplyr arrange
#' @importFrom dplyr first
#' @importFrom dplyr case_when
#' @importFrom lubridate date
#' @importFrom sf st_as_sf
#' @importFrom sf st_distance
#' @importFrom sf geometry
#' @importFrom sf st_coordinates
#' @importFrom geosphere bearing
#' 
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
#' dispSum<-dispersalSummary(ATTdata)
#'
#'@export

dispersalSummary <- function(ATTdata) {
  
  if(!inherits(ATTdata, "ATT"))
    stop("Oops! Input data needs to be an 'ATT' object.\nSet up your data first using setupData() before running this operation")
  
  ## Combine Tag.Detection and Tag.Metadata into a combined tibble for processing
  data <- 
    left_join(ATTdata$Tag.Detections, ATTdata$Tag.Metadata, by = "Transmitter") %>% 
    arrange(Tag.ID, Date.Time)

  ## Check to see if there are detections without metadata associated with them
  tags_without_metadata <- 
    data %>% 
    filter(!Transmitter %in% unique(ATTdata$Tag.Metadata$Transmitter)) %>% 
    pull(Transmitter) %>% unique()
    
  if(length(tags_without_metadata) > 0){
    message("Detections associated with the following transmitters don't have metadata associated in Tag.Metadata:")
    message(paste(tags_without_metadata, collapse = "\n"))
  }
  
  ## crs from ATTdata
  crs <- attr(ATTdata, "CRS")
  
  ## converting data into a spatial object
  data_sf <- 
    data %>% 
    st_as_sf(coords = c("Longitude", "Latitude"), crs = crs$epsg, remove = FALSE)
  
  ## Check if Release.Latitude, Release.Longitude is provided
  release_check <-
    data %>% 
    group_by(Tag.ID) %>% 
    summarise(Release.Longitude = first(Release.Longitude),
              Release.Latitude = first(Release.Latitude),
              Release.Date = first(Release.Date)) 
  
  if(any(c(is.na(release_check$Release.Latitude), is.na(release_check$Release.Longitude)))){
    message("One or more tags do not have release information associated. Using the first detection as the release location.")
    release_sf <-
      data %>%
      group_by(Tag.ID) %>% 
      mutate(first.lon = first(Longitude),
             first.lat = first(Latitude),
             first.date = date(first(Date.Time))) %>% 
      mutate(Release.Longitude = case_when(!is.na(Release.Longitude) ~ Release.Longitude, TRUE ~ first.lon),
             Release.Latitude = case_when(!is.na(Release.Latitude) ~ Release.Latitude, TRUE ~ first.lat),
             Release.Date = case_when(!is.na(Release.Date) ~ Release.Date, TRUE ~ first.date)) %>% 
      st_as_sf(coords = c("Release.Longitude", "Release.Latitude"), crs = crs$epsg, remove = FALSE)
  } else {
    release_sf <-
      data %>% 
      st_as_sf(coords = c("Release.Longitude", "Release.Latitude"), crs = crs$epsg, remove = FALSE)
  }
  
  ## New version (uses sf and geosphere)
  
  ## Straight line distance between release location and each detection
  # Release.Dispersal, Release.Bearing
  
  rel_disp <-
    data_sf %>% 
    reframe(Tag.ID,
            disp = st_distance(release_sf, data_sf, by_element = T))
  
  rel_bear <-
    data_sf %>% 
    reframe(Tag.ID,
            bear = geosphere::bearing(st_coordinates(release_sf), st_coordinates(data_sf)))
  
  
  ## Straight line distance between consecutive detections
  # Consecutive.Dispersal, Consecutive.Bearing
  con_disp <-
    data_sf %>% 
    group_by(Tag.ID) %>% 
    reframe(disp = st_distance(lag(geometry), geometry, by_element=T))
  
  con_bear <-
    data_sf %>% 
    group_by(Tag.ID) %>% 
    reframe(bear = geosphere::bearing(lag(st_coordinates(geometry)), st_coordinates(geometry)))

  ## Time since last detection
  t_last <- 
    data %>% 
    group_by(Tag.ID) %>% 
    reframe(t_last = difftime(Date.Time, lag(Date.Time), units = "secs"))

  ## Compile all datasets to return
  disptab <-
    data %>%
    mutate(Release.Dispersal = rel_disp$disp,
           Release.Bearing = rel_bear$bear,
           Consecutive.Dispersal = con_disp$disp,
           Consecutive.Bearing = con_bear$bear,
           Time.Since.Last.Detection = t_last$t_last)

  return(disptab)
  
}
