% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HRprocess.R
\name{HRprocess}
\alias{HRprocess}
\title{Preprocess activity space function}
\usage{
HRprocess(
  cenac,
  utm,
  ll,
  type = "MCP",
  h = 200,
  ext = 2,
  grid = 200,
  sub = "\%Y-\%m",
  cont = c(50, 95),
  cumulative = FALSE,
  storepoly = FALSE,
  div = 4
)
}
\arguments{
\item{cenac}{center of activity positions generated from \code{\link{COA}}, and used here to pre-process before feeding back to \code{\link{HRSummary}}}

\item{utm}{a projected coordinate reference system in meters used to accurately estimate area (see \code{\link{CRS}})}

\item{ll}{a geographic coordinate reference system in meters used to accurately estimate area (see \code{\link{CRS}})}

\item{type}{type of activity space metric to calculate. Currently "MCP" for minimum convex polygons,
"fKUD" for fixed KUD and "BBKUD" for Brownian bridge KUD. Defaults to "MCP" when no value provided.}

\item{h}{smoothing factor (in m) associated with error associated with reciever range of passive telemetry system.
Defaults to 200 m if none provided.}

\item{ext}{spatial extent used to calculate probability density metrics of activity space ('fKUD', 'BBKUD')}

\item{grid}{grid size used to calculate probability density metrics of activity space ('fKUD', 'BBKUD')}

\item{sub}{temporal subset used to calculate subsetted activity space metrics.
Currently supports monthly ('\%Y-\%m') or weekly ('\%Y-\%W'). Defaults to monthly.}

\item{cont}{contours of activity space models to estimate areas. Defaults to 50\% and 95\% contours}

\item{cumulative}{TRUE/FALSE, should the operation calculate cumulative activity space areas. Caution
this may take a long time depending on size of dataset.}

\item{storepoly}{TRUE/FALSE, should activity space metrics be saved as spatial objects (polygons or rasters)}

\item{div}{sig1 divisor used to correct BBKUD estimates.}
}
\value{
Produces a list of 2 tibbles containing Overall (full tag life) and Subsetted (user-defined temporal subsets) metrics of activity space.
  If storepoly=TRUE additional object within list containing spatial objects (MCP polygons or KUD rasters).
  Temporal subsets are currently restricted to monthly ('\%Y-\%m') or weekly ('\%Y-\%W'). Cumulative measures across temporal subsets
  included if cumulative=TRUE.
}
\description{
Internal function to run \code{\link{HRSummary}}.
}
\seealso{
Input data needs to be setup using \code{\link{setupData}}, and COAs calculated using \code{\link{COA}}.
}
