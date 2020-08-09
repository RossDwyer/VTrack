#' Calculate metrics of activity space using passive telemetry data
#'
#'@description Produce standard metrics of activity space. Metrics can include Minimum convex polygon area (MCP),
#'  fixed Kernel Utilisation Distribution (fKUD) area and Brownian Bridge KUDs (BBKUD). Activity space metrics are
#'  calculated for full period of tag life and user defined subset (currently monthly or weekly only). Cumulative
#'  activity spaces can also be estimated at set temporal subsets.
#'
#' @param COAdata a 'COA' object with estimated center of activity positions (using \code{\link{COA}})
#' @param projCRS a coordinate reference system in meters used to accurately estimate area (see \code{\link{CRS}})
#' @param type type of activity space metric to calculate. Currently "MCP" for minimum convex polygons,
#'   "fKUD" for fixed KUD and "BBKUD" for Brownian bridge KUD. Defaults to "MCP" when no value provided.
#' @param cont contours of activity space models to estimate areas. Defaults to 50\% and 95\% contours
#' @param sub temporal subset used to calculate subsetted activity space metrics.
#'   Currently supports monthly ("\%Y-\%m") or weekly ("\%Y-\%W"). Defaults to monthly.
#' @param cumulative TRUE/FALSE, should the operation calculate cumulative activity space areas. Caution
#'   this may take a long time depending on size of dataset.
#' @param storepoly TRUE/FALSE, should activity space metrics be saved as spatial objects (polygons or rasters)
#' @param h smoothing factor (in m) associated with error associated with reciever range of passive telemetry system.
#'   Defaults to 200 m if none provided.
#' @param ext spatial extent used to calculate probability density metrics of activity space ("fKUD", "BBKUD")
#' @param grid grid size used to calculate probability density metrics of activity space ("fKUD", "BBKUD")
#' @param div sig1 divisor used to correct BBKUD estimates.
#'
#' @return Produces a list of 2 tibbles containing Overall (full tag life) and Subsetted (user-defined temporal subsets) metrics of activity space.
#'   If storepoly=TRUE additional object within list containing spatial objects (MCP polygons or KUD rasters).
#'   Temporal subsets are currently restricted to monthly ("\%Y-\%m") or weekly ("\%Y-\%W"). Cumulative measures across temporal subsets
#'   included if cumulative=TRUE.
#'
#' @seealso Input data needs to be setup using \code{\link{setupData}}, and COAs calculated using \code{\link{COA}}.
#' @export
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr select
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
#' ## Estimate Short-term center of activities
#' COAdata<-COA(ATTdata)
#'
#' ## Define coordinate system for projection of detection data (in m)
#' proj<-sp::CRS("+init=epsg:3577")
#'
#' ## Estimate 100% MCP areas
#' mcp_est<-HRSummary(COAdata,
#'                    projCRS=proj, 
#'                    type="MCP", 
#'                    cont=100)
#'
#' ## Estimate 50% and 95% fKUD areas with cumulative metrics calculated
#' kud_est<-HRSummary(COAdata, 
#'                    projCRS=proj, 
#'                    type="fKUD", 
#'                    cumulative=TRUE)
#'
#' ## Estimate 20%, 50% and 95% BBKUD contour areas and store polygons
#' kud_est<-HRSummary(COAdata, 
#'                    projCRS=proj, 
#'                    type="BBKUD", 
#'                    cont=c(20,50,95), 
#'                    storepoly=TRUE)
#'
#'
HRSummary<-function(COAdata, projCRS, type="MCP", cont=c(50,95), sub='%Y-%m', cumulative=FALSE, storepoly=FALSE, h=500, ext=2, grid=200, div=4){

  TimeStep.coa <-Tag.ID <- Sci.Name <- Common.Name <- Tag.Project <- Release.Date <- Tag.Life<-Sex<-Bio<-Number.of.Detections <- NULL
  
  if(!inherits(COAdata, "COA"))
    stop("Oops! Input data needs to be a 'COA' object.\nEstimate Short-term Center of Activities first using COA() before running this operation.")
  if(!inherits(projCRS, "CRS"))
    stop("Sorry! The projected coordinate reference system should be a CRS object, see ?CRS")
  if(!sub %in% c('%Y-%m','%Y-%W'))
    stop("Hmm.. I can't recognise the temporal subset chosen.\nChoose one of the following subsets:\n\tMonthly   = '%Y-%m'\n\tWeekly  = '%Y-%W'")
  if(inherits(COAdata, "list")){
    dat<- do.call(rbind, COAdata)
    dat<- mutate(dat, subset = format(TimeStep.coa, sub))
  }else{
    dat<- COAdata %>% mutate(subset = format(TimeStep.coa, sub))
  }

  ## Define Geographic CRS (extracted from COA object) and Projected CRS (user defined)
  ll=attr(COAdata, "CRS"); utm=projCRS

  ## Set up master data frames to fill with activity space estimates
  full<- dat %>%
    group_by(Tag.ID) %>%
    summarize(Sci.Name = first(Sci.Name),
              Common.Name = first(Common.Name),
              Tag.Project = first(Tag.Project),
              Release.Date = first(Release.Date),
              Tag.Life = first(Tag.Life),
              Sex = first(Sex),
              Bio = first(Bio),
              Number.of.Detections = sum(Number.of.Detections)) %>%
    mutate(Tag.ID = as.character(Tag.ID))

  tsub<- dat %>%
    group_by(Tag.ID, subset) %>%
    summarize(Sci.Name = first(Sci.Name),
              Common.Name = first(Common.Name),
              Tag.Project = first(Tag.Project),
              Release.Date = first(Release.Date),
              Tag.Life = first(Tag.Life),
              Sex = first(Sex),
              Bio = first(Bio),
              Number.of.Detections = sum(Number.of.Detections)) %>%
    ungroup() %>%
    mutate(Tag.ID = as.character(Tag.ID))

  if(cumulative){message("You have set the operation to calculate cumulative activity space metrics. This might take some time...")}

  for(i in 1:nrow(full)){
    cenac<-filter(dat, Tag.ID %in% full$Tag.ID[i])
    tryCatch({
      prep<-HRprocess(cenac, utm=utm, ll=ll, type=type, cont=cont, sub=sub, cumulative=cumulative, storepoly=storepoly, h=h, ext=ext, grid=grid, div=div)

      if(i %in% 1){
        fullout<-prep$Full.Out
        subout<-prep$Sub.Out
        if(storepoly){
          polyout<-list()
          polyout[[full$Tag.ID[i]]]<-prep$sp
        }

      }else{
        fullout<-rbind(fullout, prep$Full.Out)
        subout<-rbind(subout, prep$Sub.Out)
        if(storepoly){
          polyout[[full$Tag.ID[i]]]<-prep$sp
        }
      }
    },error=function(e){message("\nError in Tag.ID: ", full$Tag.ID[i], "\n", conditionMessage(e))})
    setTxtProgressBar(txtProgressBar(min=0, max=nrow(full), style=3), i)
  }; cat("\n")

  ## Merge activity space metrics to master data frames
  full<-left_join(full, fullout, by=c("Tag.ID"), all.x=T)
  tsub<-left_join(tsub, subout, by=c("Tag.ID","subset"))

  if(storepoly){
    output<-list(Overall = full, Subsetted = tsub, Spatial.Objects = polyout)
  }else{
    output<-list(Overall = full, Subsetted = tsub)
  }

  return(output)
}
