\name{GenerateAnimationKMLFile_Multitag}
\alias{GenerateAnimationKMLFile_Multitag}
\title{
  Create Animation of Multiple Transmitters to View in Google Earth
}
\description{
  This function creates a Keyhole Markup Language (KML) animation of transmitter detections at receivers that can be displayed in Google Earth. The animation shows the number of transmitters detected within the detection field of a receiver on a given day. Users can adjust the time slider to visualise individual time periods for display. 
}
\usage{
  GenerateAnimationKMLFile_Multitag(sInputFile,sPointsFile,sOutputFile,
  sLocation="RECEIVERID")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sInputFile }{
    a data frame containing VTrack archive data, this archive is created using the ReadInputData function
  }
  \item{sPointsFile }{
    a data frame containing the RECEIVERID or STATIONNAME, the coordinates and the detection RADIUS in meters. This should be in the format LOCATION, LATITUDE, LONGITUDE, RADIUS
  }
  \item{sOutputFile }{
    a string detailing the location and name of the output kml file to be created
  }
    \item{sLocation }{the location at which we wish to visualise our movement events (i.e. \code{RECEIVERID} or \code{STATIONNAME}).
}}
\details{
  the output is a .kml that can be viewed as an animation in Google Earth
}

\seealso{
  \code{\link{ReadInputData}}, \code{\link{GenerateAnimationKMLFile_Track}}
}

\author{
  Ross Dwyer, Matthew Watts, Hamish Campbell
}
\examples{
  
  \dontrun{
    ###GenerateAnimationKMLFile_Multitag example
    
    # Note, users must download Google Earth in order to visualise the kml.
    
    # Load crocodile datset into VTrack archive
    data(crocs)
    data(PointsDirect_crocs)
    
    Vcrocs <- ReadInputData(infile=crocs,
                            iHoursToAdd=10,
                            dateformat = NULL,
                            sVemcoFormat='1.0')
    
    # Set working directory (in this case a temporary directory)
    setwd(tempdir())
    # or alternatively to your Desktop on Mac OS
    # setwd("~/Desktop")
    
    # Run the function to generate the KML
    GenerateAnimationKMLFile_Multitag(Vcrocs,
                                      PointsDirect_crocs,
                                      "Croc Multi.kml",
                                      sLocation="RECEIVERID")
                                      
    # This file can be found within the tempdir() directory on your computer. 
    # Double-click on the .kml file to open in Google Earth
}
}
\keyword{behaviour}
\keyword{spatial}