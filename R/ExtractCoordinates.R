#' ExtractCoordinates
#'
#' Extract the coordinates of the crosshair in terms of the poster from the video frames
#'
#' @param framedir the directory of the video frames.
#' @param poster the clear version of the poster image.
#' @param coordinate the x and y coordinates of the crosshair in terms of the
#' scene (Only two columns and no headers). NA if the coordinates are not available.
#' The default is NA.
#' @param outputcsv the directory and name of the output csv file. The default is the
#' current working directory.
#' @param outputimg the directory of the output image file, put NaN if you don't
#' want the output matching images saved. The default is the current working directory.
#' @param ibegin the starting frame.
#' @param iend the ending frame.
#' @return Returns the coordinates of the crosshair in terms of the poster and
#' a folder of the matched poster images.
#' @export ExtractData

#' @examples
#' \dontrun{
#' # Put the right version of Matlab that installed on your computer
#' library(matlabr)
#' options(matlab.path = "/Applications/MATLAB_R2016b.app/bin")
#' Check having Matlab or not
#' have_matlab()
#'
#' ExtractCoordinates(framedir = system.file("extdata", "Frames", package = "EyeTrackR"),
#' poster = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
#' coordinate = NA,
# outputcsv = "locations.csv",
# outputimg = "MatchImg",
#' ibegin = 1,
#' iend = 5)
#' file.exists("locations.csv")
#' }


#' \dontrun{
#' # Put the right version of Matlab that installed on your computer
#' options(matlab.path = "/Applications/MATLAB_R2016b.app/bin")
#' library(matlabr)
#' # Check having Matlab or not
#' have_matlab()
#'
#' ExtractCoordinates(framedir = system.file("extdata", "Frames", package = "EyeTrackR"),
#' poster = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
#' coordinate = crosshair,
# outputcsv = "locations.csv",
# outputimg = "MatchImg",
#' ibegin = 1,
#' iend = 5)
#' file.exists("locations.csv")
#' }


ExtractCoordinates <- function(framedir, poster, coordinate = NA, outputcsv = NA, outputimg = NA, ibegin, iend){

  if(dir.exists(framedir)== FALSE){
    stop('The frame directory does not exist.')
  }

  if (file.exists(poster) == FALSE){
    stop('The matching poster does not exist.')
  }

  if (ibegin <= 0){
    stop('The beginning frame index should be a positive integer.')
  }

  if(ibegin > iend){
    stop('ibegin value should be less than the iend value.')
  }

  if (is.na(outputcsv)){
    outputcsv = paste(getwd(), "/locations.csv", sep = "")
  }

  if (is.na(outputimg)){
    outputimg = paste(getwd(), "/MatchImg", sep = "")
  }


  ###
  if (is.na(coordinate)) {

    code <- c(
      paste0("cd(\'", system.file("", package = "EyeTrackR"), "\');"),
      paste0("framedir = \'", framedir, "/\';"),
      paste0("poster = \'", poster, "\';"),
      paste0("outputcsv = \'", outputcsv, "\';"),
      paste0("outputimg = \'", outputimg, "\';"),
      paste("ibegin = ", ibegin, ";"),
      paste("iend = ", iend, ";"),
      paste("LOC = ExtractData(framedir, poster, outputcsv, outputimg, ibegin, iend);")
    )

  } else {

  code <- c(
    paste0("cd(\'", system.file("", package = "EyeTrackR"), "\');"),
    paste0("framedir = \'", framedir, "/\';"),
    paste0("poster = \'", poster, "\';"),
    paste0("outputcsv = \'", outputcsv, "\';"),
    paste0("outputimg = \'", outputimg, "\';"),
    paste("ibegin = ", ibegin, ";"),
    paste("iend = ", iend, ";"),
    paste("LOC = ExtractData(framedir, poster, outputcsv, outputimg, ibegin, iend);")
           )

  }

  res = matlabr::run_matlab_code(code)

  # if(file.exists(outputcsv)== FALSE){
  #   warning('The csv file might not be sueccessfully saved or it might be saved in the R library.')
  # } else {
  #   print('The csv file has been successfully saved.')
  # }

  }
