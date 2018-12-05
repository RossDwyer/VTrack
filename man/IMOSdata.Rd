\name{IMOSdata}
\alias{IMOSdata}
\docType{data}
\title{
IMOSdata
}
\description{
This IMOS ATF dataset contains... Data supplied by....
}
\usage{data(IMOSdata)}
\format{
  A data frame with X observations on the following X variables.
  \describe{
    \item{\code{FID }}{a numeric vector containing }
    \item{\code{id }}{a numeric vector containing }    
    \item{\code{file_id }}{a numeric vector containing }
    \item{\code{url }}{a numeric vector containing }
    \item{\code{transmitter_id }}{a numeric vector containing }
    \item{\code{tag_id }}{a numeric vector containing }
    \item{\code{release_id }}{a numeric vector containing .}
    \item{\code{tag_project_name }}{a numeric vector containing .}
    \item{\code{common_name }}{the species being studied. \code{NA} suggests that no species name was supplied}
    \item{\code{scientific_name }}{the species being studied. \code{NA} suggests that no species name was supplied}   
    \item{\code{installation_name }}{a factor containing the name of the installation }
    \item{\code{station_name }}{a factor containing the name of the installatio}  
    \item{\code{receiver_name }}{a factor containing the name of the installation}    
    \item{\code{detection_timestamp }}{a factor containing }     
    \item{\code{longitude }}{a numeric vector containing the location's longitude in decimal degrees}
    \item{\code{latitude }}{a numeric vector containing the location's latitude in decimal degree}
    \item{\code{sensor_value }}{a numeric vector containing }      
    \item{\code{sensor_unit }}{a numeric vector containing } 
    \item{\code{FDA_QC }}{a numeric vector containing }
    \item{\code{Velocity_QC }}{a numeric vector containing } 
    \item{\code{Distance_QC }}{a numeric vector containing } 
    \item{\code{DetectionDistribution_QC }}{a numeric vector containing } 
    \item{\code{DistanceRelease_QC }}{a numeric vector containing } 
    \item{\code{ReleaseDate_QC }}{a numeric vector containing }     
    \item{\code{ReleaseLocation_QC }}{a numeric vector containing }         
    \item{\code{Detection_QC }}{a numeric vector containing }     
    \item{\code{release_longitude }}{a numeric vector containing } 
    \item{\code{release_latitude }}{a numeric vector containing } 
    \item{\code{ReleaseDate }}{a numeric vector containing } 
    \item{\code{sensor_slope }}{a numeric vector containing } 
    \item{\code{sensor_intercept }}{a numeric vector containing} 
    \item{\code{sensor_type }}{a numeric vector containing } 
    \item{\code{tag_sensor_unit }}{a numeric vector containing }
    \item{\code{tag_model_name }}{a numeric vector containing }   
    \item{\code{tag_serial_number }}{a numeric vector containing }   
    \item{\code{tag_expected_life_time_days }}{a numeric vector containing }  
    \item{\code{tag_status }}{a numeric vector containing}      
    \item{\code{sex }}{a numeric vector containing}      
    \item{\code{measurement }}{a numeric vector }      
    \item{\code{dual_sensor_tag }}{a numeric vector containing }     
    \item{\code{geom }}{a numeric vector containing }      
  }
}
\details{
The coordinates are given in decimal degrees WGS 84, time is GMT +10 hrs
}
\source{
\url{http://imos.org.au/facilities/animaltracking/}
}

\examples{
#load the data and print the first few rows of the data frame
data(IMOSdata)
head(IMOSdata)
}
\keyword{datasets}
