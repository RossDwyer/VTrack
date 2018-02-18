\name{COA}
\alias{COA}
\title{
Short-term Centers of Activity 
}
\description{
Function to calculate Short-term centers of activity positions from passive telemetry data. Based on technique described in: Simpfendorfer, C. A., M. R. Heupel, and R. E. Hueter. 2002. Estimation of short-term centers of activity from an array of omnidirectional hydrophones and its use in studying animal movements.  Canadian Journal of Fisheries and Aquatic Sciences 59:23-32.
}
\usage{
COA(data, id, timestep)
}
\arguments{
  \item{data }{
a data frame containing acoustic tracking data in the VEMCO VUE export format (default). file must contain Longitude Latitude information
}
  \item{id }{
a character string containing column name in the data frame for the unique tag identifier 
}
  \item{timestep }{
an integer containing the temporal bin size of center of activity calculations (in minutes)
}
}

\value{
Exports a data frame when one tag is used and a list object containing multiple data frames when multiple tag ids are used
  \item{DateTime }{a factor of the temporal bin used in the COA analysis \code{'yyyy-mm-dd HH:MM:SS'}}
  \item{Transmitter }{a character vector giving the code space and tag id code }
  \item{Transmitter.Name }{a factor containing the name of the animal as identified by the user}
  \item{Transmitter.Serial }{an numeric vector of the transmitter serial number}
  \item{Sensor.Value.coa }{a numeric vector containing the mean sensor value for a temporal bin}
  \item{Sensor.unit }{a character vector csensor unit}
  \item{Latitude.COA }{a numeric vector containing the mean Latitude value for a temporal bin)}
  \item{Longitude.COA }{a numeric vector containing the mean Longitude value for a temporal bin}
}

\author{
Vinay Udyawer, Colin Simpfendorfer
}

\keyword{archive}
\keyword{extract}
