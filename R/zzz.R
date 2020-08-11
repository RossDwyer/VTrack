## Processing functions on startup of package

.onAttach <- function(libname, pkgname){
  ver <- as.numeric(utils::packageDescription("Vtrack", fields = c("Version")))
  packageStartupMessage("VTrack version ", ver, " loaded")
  
  ## Check if an updated version of VTrack is available on GitHub
  github_url <- readLines("https://raw.githubusercontent.com/rossdwyer/VTrack/master/DESCRIPTION")
  github_ver <- as.numeric(gsub("Version:\\s*", "", github_url[grep("Version:", github_url)]))
  
  if(github_ver > ver){
    packageStartupMessage(paste0("===========================================================",
                                 "\nUpdated version of VTrack available on GitHub [ver ", github_ver,"]",
                                 "\ndownload new version using:\n",
                                 "devtools::install_github('rossdwyer/VTrack')"))
  }
}

