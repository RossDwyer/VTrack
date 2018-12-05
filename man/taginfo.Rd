\name{taginfo}
\alias{taginfo}
\docType{data}
\title{
File Containing tagged animal information 
}
\description{
This file contains the tagging data species information
}
\usage{data(taginfo)}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{transmitter_id }}{a Factor containing... }
    \item{\code{tag_id }}{a numeric vector containing }
    \item{\code{release_id }}{a numeric vector containing }
    \item{\code{tag_project_name }}{a Factor containing... }
    \item{\code{scientific_name }}{a Factor containing... }
    \item{\code{common_name }}{a Factor containing... }
    \item{\code{embargo_date }}{a Factor containing... }
    \item{\code{is_protected }}{a logical vector containing }
    \item{\code{release_longitude }}{a numeric vector containing the location's latitude (decimal degrees)}
    \item{\code{release_latitude }}{a numeric vector containing the detection radius for the location in meters}
    \item{\code{ReleaseDate }}{a Factor containing }
    \item{\code{sensor_slope }}{a numeric vector containing }
    \item{\code{sensor_intercept }}{a numeric vector containing }
    \item{\code{sensor_type }}{a Factor containing } 
    \item{\code{sensor_unit }}{a Factor containing }  
    \item{\code{tag_model_name }}{a Factor containing } 
    \item{\code{tag_serial_number }}{a numeric vector containing the tag serial number}
    \item{\code{tag_expected_life_time_days }}{a numeric vector containing the expected lifetime of the tag in days}
    \item{\code{tag_status }}{a Factor containing the status of the tag e.g. DEPLOYED}  
    \item{\code{sex }}{a Factor containing the sex of the animal if known e.g. FEMALE}  
    \item{\code{measurement }}{a Factor containing the size of the tagged animal. e.g. FORK LENGTH = 1700 mm}  
    \item{\code{dual_sensor_tag }}{a logical vector containing whether it is a dual sensor tag.}  
  }
}
\details{
The coordinates are given in decimal degrees (WGS 84).
}
\source{
\url{http://imos.org.au/facilities/animaltracking/}
}

\examples{
data(taginfo)
head(taginfo)


}
\keyword{taginfo}
