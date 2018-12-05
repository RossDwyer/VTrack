#' Preprocess activity space function
#'
#'@description Internal function to run \code{\link{HRSummary}}.
#'
#' @param COAdata a 'COA' object with estimated center of activity positions (using \code{\link{COA}})
#' @param utm a projected coordinate reference system in meters used to accurately estimate area (see \code{\link{CRS}})
#' @param ll a geographic coordinate reference system in meters used to accurately estimate area (see \code{\link{CRS}})
#' @param type type of activity space metric to calculate. Currently "MCP" for minimum convex polygons,
#'   "fKUD" for fixed KUD and "BBKUD" for Brownian bridge KUD. Defaults to "MCP" when no value provided.
#' @param cont contours of activity space models to estimate areas. Defaults to 50\% and 95\% contours
#' @param sub temporal subset used to calculate subsetted activity space metrics.
#'   Currently supports monthly ('\%Y-\%m') or weekly ('\%Y-\%W'). Defaults to monthly.
#' @param cumulative TRUE/FALSE, should the operation calculate cumulative activity space areas. Caution
#'   this may take a long time depending on size of dataset.
#' @param storepoly TRUE/FALSE, should activity space metrics be saved as spatial objects (polygons or rasters)
#' @param h smoothing factor (in m) associated with error associated with reciever range of passive telemetry system.
#'   Defaults to 200 m if none provided.
#' @param ext spatial extent used to calculate probability density metrics of activity space ('fKUD', 'BBKUD')
#' @param grid grid size used to calculate probability density metrics of activity space ('fKUD', 'BBKUD')
#' @param div sig1 divisor used to correct BBKUD estimates.
#'
#' @return Produces a list of 2 tibbles containing Overall (full tag life) and Subsetted (user-defined temporal subsets) metrics of activity space.
#'   If storepoly=TRUE additional object within list containing spatial objects (MCP polygons or KUD rasters).
#'   Temporal subsets are currently restricted to monthly ('\%Y-\%m') or weekly ('\%Y-\%W'). Cumulative measures across temporal subsets
#'   included if cumulative=TRUE.
#'
#' @seealso Input data needs to be setup using \code{\link{setupData}}, and COAs calculated using \code{\link{COA}}.
#' @export
#' @import adehabitatHR
#' @import sp
#' @importFrom raster spTransform
#' @importFrom raster projection
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr select
#' @importFrom lubridate ymd_hms
HRprocess <- function(cenac, utm, ll, type="MCP", h=200, ext=2, grid=200, sub="%Y-%m", cont=c(50,95), cumulative=FALSE, storepoly=FALSE, div=4){

  TimeStep.coa <- Latitude.coa <- Longitude.coa <- V1 <- Tag.ID <- raster <- NULL 
  
  ## add subset column
  cenac <- mutate(cenac, subset = factor(format(TimeStep.coa, sub)))
  
  ### remove subsets with fewer than 5 detections
  COA <- droplevels(cenac[cenac$subset %in% names(table(cenac$subset)[table(cenac$subset) > 5]), ])
  
  if (nrow(unique(COA[, c("Latitude.coa", "Longitude.coa")])) > 5) {
    ## Setup spatial data and convert from lat long to UTM
    sdat <- COA
    coordinates(sdat) <- c("Longitude.coa", "Latitude.coa")
    projection(sdat) <- ll
    dat <- spTransform(sdat, utm)
    
    if (type %in% "MCP") {
      ## full tag life
      fullout <-
        as.data.frame(matrix(
          NA,
          ncol = length(cont) + 1,
          nrow = 1,
          dimnames = list(c(), c("Tag.ID", paste0("MCP.", cont)))
        ))
      fullout[, "Tag.ID"] <- levels(dat$Tag.ID)[1]
      for (m in 1:length(cont)) {
        fullout[1, paste0("MCP.", cont[m])] <-
          mcp(dat[, "Tag.ID"],
              percent = cont[m],
              unin = "m",
              unout = "m2")@data$area
      }
      ## temporal subset
      subout <-
        as.data.frame(matrix(
          NA,
          ncol = length(cont) + 2,
          nrow = length(levels(dat$subset)),
          dimnames = list(c(), c(
            "Tag.ID", "subset", paste0("MCP.", cont)
          ))
        ))
      subout[, "Tag.ID"] <-
        levels(dat$Tag.ID)[1]
      subout[, "subset"] <- levels(dat$subset)
      for (m in 1:length(cont)) {
        subout[, paste0("MCP.", cont[m])] <-
          mcp(dat[, "subset"],
              percent = cont[m],
              unin = "m",
              unout = "m2")@data$area
      }
      
      ## cumulative area estimation
      if (cumulative) {
        cumout <-
          as.data.frame(matrix(
            NA,
            ncol = length(cont) + 2,
            nrow = length(levels(dat$subset)),
            dimnames = list(c(), c(
              "Tag.ID", "subset", paste0("Cumulative.MCP.", cont)
            ))
          ))
        cumout[, "Tag.ID"] <-
          levels(dat$Tag.ID)[1]
        cumout[, "subset"] <- levels(dat$subset)
        for (c in 1:length(levels(dat$subset))) {
          cdat <- subset(dat, subset %in% levels(dat$subset)[1:c])
          for (m in 1:length(cont)) {
            cumout[c, paste0("Cumulative.MCP.", cont[m])] <-
              mcp(cdat[, "Tag.ID"],
                  percent = cont[m],
                  unin = "m",
                  unout = "m2")@data$area
          }
        }
        subout <-
          left_join(subout, cumout, by = c("Tag.ID", "subset"))
      }
      
      output <- list(Full.Out = fullout, Sub.Out = subout)
      
      ## Storing polygons
      if (storepoly) {
        Spatial.Objects <- list()
        tryCatch({
          for (m in 1:length(cont)) {
            mcpcont <-
              mcp(dat[, "Tag.ID"],
                  percent = cont[m],
                  unin = "m",
                  unout = "m2")
            if (m %in% 1) {
              mcp_full <-
                spTransform(mcpcont, ll)
            } else{
              mcp_full <- rbind(mcp_full, spTransform(mcpcont, ll))
            }
          }
          Spatial.Objects$MCP_full <- mcp_full
        }, error = function(e) {
          message(
            "Error in saving full MCP Polygons for Tag.ID: ",
            cenac$Tag.ID[1],
            "\n",
            conditionMessage(e)
          )
        })
        
        tryCatch({
          for (s in 1:length(levels(dat$subset))) {
            for (c in 1:length(cont)) {
              mcpcont <-
                mcp(
                  dat[dat$subset %in% levels(dat$subset)[s], "Tag.ID"],
                  percent = cont[c],
                  unin = "m",
                  unout = "m2"
                )
              if (c %in% 1) {
                mcp_sub <-
                  spTransform(mcpcont, ll)
              } else{
                mcp_sub <- rbind(mcp_sub, spTransform(mcpcont, ll))
              }
            }
            mcp_sub$id <- cont
            if (s %in% 1) {
              sub_list <- list()
              sub_list[[levels(dat$subset)[s]]] <- mcp_sub
            } else{
              sub_list[[levels(dat$subset)[s]]] <- mcp_sub
            }
          }
          Spatial.Objects$MCP_sub <- sub_list
        }, error = function(e) {
          message(
            "Error in saving subsetted MCP Polygon list for Tag.ID: ",
            cenac$Tag.ID[1],
            "\n",
            conditionMessage(e)
          )
        })
        
        output <-
          list(Full.Out = fullout,
               Sub.Out = subout,
               sp = Spatial.Objects)
        
      }
    }
    
    
    if (type %in% "BBKUD") {
      ### Define grid
      width <-
        ceiling(max((extent(dat)[2] - extent(dat)[1]) / 2, (extent(dat)[4] - extent(dat)[3]) /
                      2))
      xcen <-
        (extent(dat)[2] + extent(dat)[1]) / 2
      ycen <- (extent(dat)[4] + extent(dat)[3]) / 2
      gr <-
        expand.grid(x = seq(xcen - (width * ext), xcen + (width * ext), len = grid),
                    y = seq(ycen - (width * ext), ycen + (width * ext), len = grid))
      coordinates(gr) <- ~ x + y
      gridded(gr) <- TRUE
      
      ## full tag life
      fullout <-
        as.data.frame(matrix(
          NA,
          ncol = length(cont) + 1,
          nrow = 1,
          dimnames = list(c(), c("Tag.ID", paste0("BBKUD.", cont)))
        ))
      fullout[, "Tag.ID"] <- levels(dat$Tag.ID)[1]
      tf <-
        as.ltraj(
          xy = coordinates(dat),
          date = dat$TimeStep.coa,
          id = dat$Tag.ID,
          typeII = TRUE,
          proj4string = utm
        )
      s1f <-
        (liker(
          tf,
          rangesig1 = c(0, 500),
          sig2 = h,
          byburst = FALSE,
          plotit = FALSE
        )[[1]]$sig1) / div
      tryCatch({
        kbfull <- kernelbb(tf,
                           sig1 = s1f,
                           sig2 = h,
                           grid = gr)
        bf <- kernel.area(kbfull,
                          percent = cont,
                          unin = "m",
                          unout = "m2")
        fullout[, paste0("BBKUD.", cont)] <- bf
      }, error = function(e) {
        message(
          "Error in calculating full BBKUD estimates for Tag.ID: ",
          cenac$Tag.ID[1],
          "\n",
          conditionMessage(e)
        )
      })
      
      ## temporal subset
      subout <-
        data.frame(matrix(
          NA,
          ncol = 2,
          nrow = length(levels(dat$subset)),
          dimnames = list(c(), c("Tag.ID", "subset"))
        ))
      subout[, "Tag.ID"] <-
        levels(dat$Tag.ID)[1]
      subout[, "subset"] <- levels(dat$subset)
      ## identify subsets where more than 5 unique coorindates exist for smoother kud estimation
      ym <-
        COA %>% group_by(subset) %>% summarise(V1 = n_distinct(Latitude.coa, Longitude.coa)) %>% filter(V1 > 5) %>% data.frame()
      yfac <- ym[ym$V1 > 5, "subset"]
      
      if (length(yfac) > 0) {
        traj <-
          as.ltraj(
            xy = coordinates(dat),
            date = dat$TimeStep.coa,
            id = dat$subset,
            typeII = TRUE,
            proj4string = utm
          )[yfac]
        s1 <-
          liker(
            traj,
            rangesig1 = c(0, 500),
            sig2 = h,
            byburst = FALSE,
            plotit = FALSE
          )
        sig1 <- (unname(sapply(s1, '[[', 1))) / div
        tryCatch({
          kbb <- kernelbb(traj,
                          sig1 = sig1,
                          sig2 = h,
                          grid = gr)
          bb <- kernel.area(kbb,
                            percent = cont,
                            unin = "m",
                            unout = "m2")
        }, error = function(e) {
          message(
            "Error in calculating subsetted BBKUD estimates for Tag.ID: ",
            cenac$Tag.ID[1],
            "\n",
            conditionMessage(e)
          )
        })
        
        barea <-
          data.frame(Tag.ID = as.character(levels(dat$Tag.ID)[1]) ,
                     subset = as.character(yfac),
                     t(data.frame(bb)))
        colnames(barea) <-
          c("Tag.ID", "subset", paste0("BBKUD.", cont))
        rownames(barea) <- NULL
        subout <-
          left_join(
            subout,
            barea %>% mutate(Tag.ID = as.character(Tag.ID), subset = as.character(subset)),
            by = c("Tag.ID", "subset")
          )
      }
      
      ## cumulative area estimation
      if (cumulative) {
        cumout <-
          as.data.frame(matrix(
            NA,
            ncol = length(cont) + 2,
            nrow = length(levels(dat$subset)),
            dimnames = list(c(), c(
              "Tag.ID", "subset", paste0("Cumulative.BBKUD.", cont)
            ))
          ))
        cumout[, "Tag.ID"] <-
          levels(dat$Tag.ID)[1]
        cumout[, "subset"] <- levels(dat$subset)
        for (c in 1:length(levels(dat$subset))) {
          cdat <- subset(dat, subset %in% levels(dat$subset)[1:c])
          ct <-
            as.ltraj(
              xy = cdat@coords,
              date = cdat$TimeStep.coa,
              id = cdat$Tag.ID,
              typeII = TRUE,
              proj4string = utm
            )
          csig1 <-
            (liker(
              ct,
              rangesig1 = c(0, 500),
              sig2 = h,
              byburst = FALSE,
              plotit = FALSE
            )[[1]]$sig1) / div
          tryCatch({
            ckbb <- kernelbb(ct,
                             sig1 = csig1,
                             sig2 = h,
                             grid = gr)
            cumout[c, paste0("Cumulative.BBKUD.", cont)] <-
              kernel.area(ckbb,
                          percent = cont,
                          unin = "m",
                          unout = "m2")
          }, error = function(e) {
            message(
              "Error in calculating cumulative BBKUD estimates for Tag.ID: ",
              cenac$Tag.ID[1],
              "\n",
              conditionMessage(e)
            )
          })
        }
        subout <-
          left_join(subout, cumout, by = c("Tag.ID", "subset"))
      }
      
      output <- list(Full.Out = fullout, Sub.Out = subout)
      
      ## Storing polygons
      if (storepoly) {
        Spatial.Objects <- list()
        tryCatch({
          ras_full <- raster(getvolumeUD(kbfull))
          projection(ras_full) <- utm
          Spatial.Objects$BBKUD_full <-
            projectRaster(ras_full, crs = ll)
        }, error = function(e) {
          message(
            "Error in saving full BBKUD raster for Tag.ID: ",
            cenac$Tag.ID[1],
            "\n",
            conditionMessage(e)
          )
        })
        tryCatch({
          ras_sub <-
            ras <-
            stack(lapply(getvolumeUD(kbb), raster))
          projection(ras_sub) <- utm
          Spatial.Objects$BBKUD_sub <-
            projectRaster(ras_sub, crs = ll)
        }, error = function(e) {
          message(
            "Error in saving subsetted BBKUD rasterstack for Tag.ID: ",
            cenac$Tag.ID[1],
            "\n",
            conditionMessage(e)
          )
        })
        
        output <-
          list(Full.Out = fullout,
               Sub.Out = subout,
               sp = Spatial.Objects)
        
      }
    }
    
    if (type %in% "fKUD") {
      ### Define grid
      width <-
        ceiling(max((extent(dat)[2] - extent(dat)[1]) / 2, (extent(dat)[4] - extent(dat)[3]) /
                      2))
      xcen <-
        (extent(dat)[2] + extent(dat)[1]) / 2
      ycen <- (extent(dat)[4] + extent(dat)[3]) / 2
      gr <-
        expand.grid(x = seq(xcen - (width * ext), xcen + (width * ext), len = grid),
                    y = seq(ycen - (width * ext), ycen + (width * ext), len = grid))
      coordinates(gr) <- ~ x + y
      gridded(gr) <- TRUE
      
      ## full tag life
      fullout <-
        as.data.frame(matrix(
          NA,
          ncol = length(cont) + 1,
          nrow = 1,
          dimnames = list(c(), c("Tag.ID", paste0("fKUD.", cont)))
        ))
      fullout[, "Tag.ID"] <- levels(dat$Tag.ID)[1]
      tryCatch({
        kbfull <- kernelUD(dat[c("Tag.ID")], h = h, grid = gr)[[1]]
        bf <- kernel.area(kbfull,
                          percent = cont,
                          unin = "m",
                          unout = "m2")
        fullout[, paste0("fKUD.", cont)] <- bf
      }, error = function(e) {
        message(
          "Error in calculating full fKUD estimates for Tag.ID: ",
          cenac$Tag.ID[1],
          "\n",
          conditionMessage(e)
        )
      })
      
      ## temporal subset
      subout <-
        data.frame(matrix(
          NA,
          ncol = 2,
          nrow = length(levels(dat$subset)),
          dimnames = list(c(), c("Tag.ID", "subset"))
        ))
      subout[, "Tag.ID"] <-
        levels(dat$Tag.ID)[1]
      subout[, "subset"] <- levels(dat$subset)
      ## identify subsets where more than 5 unique coorindates exist for smoother kud estimation
      ym <-
        COA %>% group_by(subset) %>% summarise(V1 = n_distinct(Latitude.coa, Longitude.coa)) %>% filter(V1 > 5) %>% data.frame()
      yfac <- ym[ym$V1 > 5, "subset"]
      
      kdat <-
        dat[dat$subset %in% yfac, ]
      kdat$subset <- droplevels(kdat$subset)
      
      if (length(yfac) > 0) {
        tryCatch({
          kbb <- kernelUD(kdat[c("subset")], h = h, grid = gr)
          bb <- kernel.area(kbb,
                            percent = cont,
                            unin = "m",
                            unout = "m2")
        }, error = function(e) {
          message(
            "Error in calculating subsetted fKUD estimates for Tag.ID: ",
            cenac$Tag.ID[1],
            "\n",
            conditionMessage(e)
          )
        })
        
        barea <-
          data.frame(Tag.ID = as.character(levels(dat$Tag.ID)[1]) ,
                     subset = as.character(yfac),
                     t(data.frame(bb)))
        colnames(barea) <-
          c("Tag.ID", "subset", paste0("fKUD.", cont))
        rownames(barea) <- NULL
        subout <-
          left_join(
            subout,
            barea %>% mutate(Tag.ID = as.character(Tag.ID), subset = as.character(subset)),
            by = c("Tag.ID", "subset")
          )
      }
      
      ## cumulative area estimation
      if (cumulative) {
        cumout <-
          as.data.frame(matrix(
            NA,
            ncol = length(cont) + 2,
            nrow = length(levels(dat$subset)),
            dimnames = list(c(), c(
              "Tag.ID", "subset", paste0("Cumulative.fKUD.", cont)
            ))
          ))
        cumout[, "Tag.ID"] <-
          levels(dat$Tag.ID)[1]
        cumout[, "subset"] <- levels(dat$subset)
        for (c in 1:length(levels(dat$subset))) {
          cdat <- subset(dat, subset %in% levels(dat$subset)[1:c])
          tryCatch({
            ckbb <- kernelUD(cdat[c("Tag.ID")], h = h, grid = gr)[[1]]
            cumout[c, paste0("Cumulative.fKUD.", cont)] <-
              kernel.area(ckbb,
                          percent = cont,
                          unin = "m",
                          unout = "m2")
          }, error = function(e) {
            message(
              "Error in calculating cumulative fKUD estimates for Tag.ID: ",
              cenac$Tag.ID[1],
              "\n",
              conditionMessage(e)
            )
          })
        }
        subout <-
          left_join(subout, cumout, by = c("Tag.ID", "subset"))
      }
      
      output <- list(Full.Out = fullout, Sub.Out = subout)
      
      
      ## Storing polygons
      if (storepoly) {
        Spatial.Objects <- list()
        tryCatch({
          ras_full <- raster(getvolumeUD(kbfull))
          projection(ras_full) <- utm
          Spatial.Objects$fKUD_full <-
            projectRaster(ras_full, crs = ll)
        }, error = function(e) {
          message(
            "Error in saving full fKUD raster for Tag.ID: ",
            cenac$Tag.ID[1],
            "\n",
            conditionMessage(e)
          )
        })
        tryCatch({
          ras_sub <-
            ras <-
            stack(lapply(getvolumeUD(kbb), raster))
          projection(ras_sub) <- utm
          Spatial.Objects$fKUD_sub <-
            projectRaster(ras_sub, crs = ll)
        }, error = function(e) {
          message(
            "Error in saving subsetted fKUD rasterstack for Tag.ID: ",
            cenac$Tag.ID[1],
            "\n",
            conditionMessage(e)
          )
        })
        
        output <-
          list(Full.Out = fullout,
               Sub.Out = subout,
               sp = Spatial.Objects)
        
      }
    }
  } else{
    # warning('Somethings wrong! There might not be enough unique relocations to estimate Activity Space metrics')
    if (storepoly) {
      output <-
        list(Full.Out = NA,
             Sub.Out = NA,
             sp = NA)
    } else{
      output <- list(Full.Out = NA, Sub.Out = NA)
    }
  }
  
  return(output)
}
