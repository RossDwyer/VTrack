## Processing functions on startup of package

.onLoad <- function(libname, pkgname){
  invisible(suppressPackageStartupMessages(
    sapply(c("tibble", "purrr", "dplyr", "tidyr", "ggplot2", "data.table", "adehabitatMA", "foreach", "parallel", "doParallel"),
           requireNamespace, quietly = TRUE)
  ))
  ver <- utils::packageVersion("VTrack")
  packageStartupMessage("Loading VTrack version ", ver)
}

