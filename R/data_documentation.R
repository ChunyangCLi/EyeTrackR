
#'
#' locations_testing
#'
#' The coordinates returned by Matlab from the data collectd from looking at the example poster:
#' the participant looked at the logo, title and author, image top left and image bottom middle
#' for two seconds and the rest of the area of interests for six seconds.
#' The viewing sequence is: logo, title and author, image top right, data set information, accuracy tables,
#' conclusion, introduction, image bottomn middle, confusion matrices, variable importance, classification,
#' and acknowledgements.
#'
#'
#' @docType data
#'
#' @usage data(locations_testing)
#'
#' @format A data frame with 1647 rows and 3 variables.
#'
#'
#' \describe{
#' \item{V1}{x coordinates}
#' \item{V2}{y coordinates}
#' \item{V3}{pupil radius in pixels}
#' }
#'
#' @keywords datasets
#'
"locations_testing"


#'
#' AOIName
#'
#' The defined Area of Interest information with function \code{DrawAOIs}
#'
#'
#' @docType data
#'
#' @usage data(AOIName)
#'
#'@format A data frame with 72 rows and 3 variables.
#' \describe{
#' \item{section}{name of AOI}
#' \item{x}{x coordinates of the borders}
#' \item{y}{y coordinates of the borders}
#' }
#'
#' @keywords datasets
#'
"AOIName"


#'
#' crosshair
#'
#' The x and y coordinates of the crosshair in the scene.
#'
#'
#' @docType data
#'
#' @usage data(crosshair)
#'
#'@format A data frame with 5 rows and 2 variables.
#' \describe{
#' \item{V1}{the x coordinate of the crosshair in the scene.}
#' \item{V2}{the y coordinate of the crosshair in the scene.}
#' }
#'
#' @keywords datasets
#'
"crosshair"



#'
#' posterdat.all
#'
#' A list of summarized eye movement statics with function \code{get.posterdat}.
#'
#'
#' @docType data
#'
#' @usage data(posterdat.all)
#'
#'@format A list with three lists.
#' \describe{
#' \item{[1]}{a dataframe with three columns:}
#' \describe{
#' \item{section}{AOI names.}
#' \item{visits}{time the participant spent on each AOI (in seconds).}
#' \item{visits.num}{how many times the participant has visited each AOI.}
#' }
#' \item{[2]}{a list of eye movement speed for each AOIs (each AOI is a list).}
#' \item{[3]}{A list of pupil radius for each AOIs (each AOI is a list).}
#' }
#'
#' @keywords datasets
#'
"posterdat.all"


#'
#' scanpathdat.all
#'
#' A list of summarized eye movement statics with function \code{get.scanpathdat}.
#'
#'
#' @docType data
#'
#' @usage data(scanpathdat.all)
#' @format A list with three lists.
#' \describe{
#' \item{[1]}{a dataframe with three columns:}
#' \describe{
#' \item{section}{AOI names.}
#' \item{visits}{time the participant spent on each AOI (in seconds).}
#' \item{visits.num}{how many times the participant has visited each AOI.}
#' }
#' \item{[2]}{a list of eye movement speed for each visit (each visit is a list).}
#' \item{[3]}{a list of list with data of people looking at the areas that are not defined
#' as an AOI:}
#' \describe{
#' \item{[1]}{a list with two elements: time spent in the areas and the number of visits for the areas.}
#' \item{[2]}{a list of eye movement speed in the areas.}
#' \item{[3]}{a list of pupil radius in the areas.}
#' }
#' \item{[4]}{a list of pupil radius for each visit (each visit is a list).}
#' }
#'
#' @keywords datasets
#'
"scanpathdat.all"








