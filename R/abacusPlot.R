#' Plot daily detections over time (or across recievers)
#'
#' @description Produces an abacus plot to visualise patterns of detection over time or across signal receivers
#'
#' @param ATTdata input 'ATT' object containing tag detection, metadata and station information.
#' @param id input Tag.ID for a single or several tags within the 'ATT' object to plot
#' @param theme ggplot theme used in plot (default = "theme_linedraw")
#' @param xlab x axis label
#' @param ylab y axis label
#' @param det.col color of points indicating detections (default = "red")
#' @param tag.col color of points indicating start and end of tag life (default = "grey")
#' @param facet conditional argument to plot faceted plot, plotting detections by each animal on receiver stations (default = FALSE)
#' @param new.window conditional argument for plotting in a new window (default = FALSE)
#' @param ... other arguments sent to geom_points to modify plotting aesthetics
#'
#' @return Produces an abacus plot in a new window. Daily detections over time for each tag
#'   (determined by "Tag.ID" field name). If facet parameter is TRUE detections of each tag on
#'   receiver stations plotted.
#' @seealso Input data needs to be setup using \code{\link{setupData}}
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @import ggplot2
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
#' ## Create abacus plot
#' abacusPlot(ATTdata)
#'
#'
#' @export

abacusPlot <- function(ATTdata,
                       id = NULL,
                       theme = "theme_linedraw",
                       xlab = NULL,
                       ylab = NULL,
                       det.col = "grey",
                       tag_start.col = "forestgreen",
                       tag_end.col = "firebrick",
                       facet = FALSE,
                       new.window = FALSE,
                       ...) {
  
  if(!inherits(ATTdata, "ATT"))
    stop("Oops! Input data needs to be an 'ATT' object.
         \nSet up your data first using setupData() before running this operation")

  
  ## Combine Tag.Detection and Tag.Metadata into a combined tibble for plotting
  combdata <- left_join(ATTdata$Tag.Detections, ATTdata$Tag.Metadata, by = "Transmitter")

  ## Subset Tag.ID if 'id' is supplied
  if(!is.null(id)){
    combdata <- combdata %>%
      filter(Tag.ID %in% id)
  }

  ## Find start and end date of taglife
  ss <- 
    combdata %>%
    group_by(Tag.ID = factor(Tag.ID)) %>%
    summarise(Start = first(Release.Date),
              End = first(Release.Date) + first(Tag.Life))
  
  if(any(is.na(ss[,c("Start", "End")]))){
    message("One or more tags don't have a Release date or Estimated tag life information associated.")
  }  
  
  if(new.window){dev.new(noRStudioGD=TRUE, width=9, height=6)}

  if(facet){
    combdata %>%
      group_by(Date.Time = date(Date.Time), Tag.ID = factor(Tag.ID), Station.Name = factor(Station.Name)) %>%
      summarise(num_det = n(), .groups = "keep") %>%
      ggplot() +
      xlab(ifelse(!is.null(xlab), xlab, "Date")) + ylab(ifelse(!is.null(ylab), ylab, "Station Name")) +
      geom_point(aes(x = date(Date.Time), y = as.factor(Station.Name)), col = det.col, ...) +
      geom_vline(data = ss, aes(xintercept = Start), col = tag_start.col, na.rm = T) +
      geom_vline(data = ss, aes(xintercept = End), col = tag_end.col, na.rm = T) +
      facet_wrap(~factor(Tag.ID)) +
      scale_x_date(date_labels = "%b\n%Y", minor_breaks = NULL) +
      eval(call(theme))
  }else{
    combdata %>%
      group_by(Date.Time = date(Date.Time), Tag.ID = factor(Tag.ID)) %>%
      summarise(num_det = n(), .groups = "keep") %>%
      ggplot() +
      xlab(ifelse(!is.null(xlab), xlab, "Date")) + ylab(ifelse(!is.null(ylab), ylab, "Tag ID")) +
      geom_point(aes(x = date(Date.Time), y = as.factor(Tag.ID)), col = det.col, ...) +
      geom_point(data = ss, aes(x = Start, y = as.factor(Tag.ID)), pch = "|", col = tag_start.col, cex = 3, na.rm = T) +
      geom_point(data = ss, aes(x = End, y = as.factor(Tag.ID)), pch = "|", col = tag_end.col, cex = 3, na.rm = T) +
      scale_x_date(date_labels = "%b\n%Y", minor_breaks = NULL) +
      eval(call(theme))
  }
}
