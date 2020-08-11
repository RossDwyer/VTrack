## Processing functions on startup of package

.onAttach <- function(libname, pkgname){
  ver <- utils::packageVersion("VTrack")
  packageStartupMessage("VTrack version ", ver, " loaded")
  
  ## Check if an updated version of VTrack is available on GitHub
  github_url <- readLines("https://raw.githubusercontent.com/rossdwyer/VTrack/master/DESCRIPTION")
  github_ver <- gsub("Version:\\s*", "", github_url[grep("Version:", x)])
  
  if(github_ver > ver){
    packageStartupMessage(paste0("Updated version of VTrack available on GitHub [ver ", github_ver,"]",
                                 "\ndownload new version using\n",
                                 "devtools::install_github('rossdwyer/VTrack')"))
  }
}

