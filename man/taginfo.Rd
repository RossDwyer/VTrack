\name{taginfo}
\alias{taginfo}
\docType{data}
\title{
Example dataset containing tag metadata associated with \code{IMOSdata} and \code{VEMCOdata} datasets
}
\description{
This dataset contains an example of tag metadata required for the functioning of the ATT operations. The example tags here is associated with Tiger sharks monitored at Heron and One Tree Island, Australia (see \code{IMOSdata} or \code{VEMCOdata}). The data is in the default format used by the IMOS ATF containing the metadata associated with transmitter deployments.
}
\usage{data(taginfo)}
\format{
  A data frame with 11 observations on the following 22 variables.
  \describe{
    \item{\code{transmitter_id }}{a factor containing the combination of code map and ping ID for each transmitter deployed (e.g. A69-9002-12345). Note: Dual sensor tags are often associated with multiple transmitter IDs. Required field. }
    \item{\code{tag_id }}{a numeric vector containing }
    \item{\code{release_id }}{a factor unique ID for each tagged individual (e.g. 4462716, "Tigger"). Note: This field unifies detections for animals tagged with dual sensor tags that have multiple transmitter_id codes. Required field. }
    \item{\code{tag_project_name }}{a factor containing the name of the project associated with each transmitter deployment. Not required for the functioning of the ATT operations. }
    \item{\code{scientific_name }}{the scientific name for the species being studied. \code{NA} suggests that no species name was supplied. Not required for the functioning of the ATT operations. }
    \item{\code{common_name }}{the common name for the species being studied. \code{NA} suggests that no species name was supplied. Not required for the functioning of the ATT operations. }
    \item{\code{embargo_date }}{a date vector containing any data embargo time period. \code{NA} suggests that no embargo was supplied on the tag. }
    \item{\code{is_protected }}{a logical vector indicating if the data from this tag is protected. Protected status indicates restrictions on data use. Contact researcher or organisation that deployed this tag to discuss data use. }
    \item{\code{release_longitude }}{a numeric vector containing the longitude at which transmitter was deployed (d.ddd˚). }
    \item{\code{release_latitude }}{a numeric vector containing the latitude at which transmitter was deployed (d.ddd˚). }
    \item{\code{ReleaseDate }}{a vector of type POSIXct containing date and time at which tag was deployed (yyyy-mm-dd HH:MM:SS). }
    \item{\code{sensor_slope }}{a numeric vector containing the slope used in the linear equation to convert raw sensor measurements. This value is provided by transmitter manufacturer. } 
    \item{\code{sensor_intercept }}{a numeric vector containing the intercept used in the linear equation to convert raw sensor measurements. This value is provided by transmitter manufacturer. } 
    \item{\code{sensor_type }}{a character vector indicating type of sensor (Can be: pinger, temperature, pressure, or accelerometer). } 
    \item{\code{tag_sensor_unit }}{a character vector conatining the units pertaining to the sensor value (e.g. m, ms-2, degrees celcius). }
    \item{\code{tag_model_name }}{a character vector indicating tag model (e.g. V9, V13-TP, V16-P, V9-A). }   
    \item{\code{tag_serial_number }}{a numeric vector containing manufacturers serial number for deployed transmitter (e.g. 1083971). }   
    \item{\code{tag_expected_life_time_days }}{a numeric vector containing the expected life time of the transmitter (days). This estimated battery life is provided by transmitter manufacturer. }  
    \item{\code{tag_status }}{a character vector indicating the status of transmitter (e.g. deployed, lost, etc). }      
    \item{\code{sex }}{a character vector containing sex of tagged animal, \code{NA} suggests that sex of tagged animal was not supplied. }      
    \item{\code{measurement }}{a numeric vector containing morphometric measurement of tagged animal (e.g. Fork length, Stretch total length, Carapace width, Snout-vent length), \code{NA} suggests that measurement of tagged animal was not supplied. }      
    \item{\code{dual_sensor_tag }}{a logical vector indicating if the transmitter was a dual sensor tag. }   
  }
}
\details{
The coordinates are given in decimal degrees WGS 84, time is in UTC/GMT.
}
\source{
\url{https://animaltracking.aodn.org.au}
}

\examples{
## load the data and print the first few rows of the data frame
data(taginfo)
head(taginfo)


}
\keyword{taginfo}
