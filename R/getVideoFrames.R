

#' GetVideoFrames
#'
#'
#' Break the video into individual frames
#'
#' @param video the eye tracking video that can be read by matlab VideoReader.
#' @param start the starting second of the extracted frames. Default is 0.
#' @param end the ending second of the extracted frames, default is the end time of video.
#' @param step the interval in seconds between frames, default is 1/30.
#' @param savetodir the output directory, default is the current directory.
#' @return Returns the video frames extracted.
#' @export GetVideoFrames
#' @examples
#' \dontrun{
#' # Put the right version of Matlab that installed on your computer
#' options(matlab.path = "/Applications/MATLAB_R2016b.app/bin")
#' # Check having Matlab or not
#' library(matlabr)
#' have_matlab()
#' # Call function
#' GetVideoFrames(system.file("extdata", "recording.avi", package = "EyeTrackR"),
#'  0, 2, 1/30)
#' }
#'
#' @author Chunyang Li < lichunyang1990@hotmail.com >
#' @references  See \url{https://www.mathworks.com/matlabcentral/fileexchange/46615-getvideoframes-vid--startt--endt--step--savetodir-}.


GetVideoFrames <- function(video, start = 0, end = 99999, step = 1/30, savetodir = NA){

  if (file.exists(video) == FALSE){
    stop('The avi file does not exist.')
  }

  if (start < 0){
    stop('start should be non negative.')
  }

  if(start > end){
    stop('start value should be less than the end value.')
  }

  if (step < 0){
    stop('step should be non negative.')
  }

  if (is.na(savetodir)){
    savetodir =  paste(getwd(), "/Frames", sep = "")
  }

  if(dir.exists(savetodir)== FALSE){
    stop('The output directory does not exist.')
  }

#
#   code = c( paste0("cd(\'", system.file("", package = "EyeTrackR"), "\');"),
#             paste0("imgs = getVideoFrames(", "\'", video, "\'", ",", start, ",", end, ",", step, ",", "\'", savetodir, "\');")
#   )

  code = c( paste0("cd(\'", system.file("", package = "EyeTrackR"), "\');"),
            paste0("imgs = BreakVideoCall(", "\'", video, "\'", ",", start, ",", end, ",", step, ",", "\'", savetodir, "\');")
  )

  res = matlabr::run_matlab_code(code)

}





