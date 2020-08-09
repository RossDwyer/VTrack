## Processing functions on startup of package

.onAttach <- function(libname, pkgname){
  ver <- utils::packageVersion("VTrack")
  packageStartupMessage("Loading VTrack version ", ver)
}

