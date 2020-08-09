## Processing functions on startup of package

.onLoad <- function(libname, pkgname){
  ver <- utils::packageVersion("VTrack")
  packageStartupMessage("VTrack version ", ver, " loaded")
}

