% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getVideoFrames.R
\name{GetVideoFrames}
\alias{GetVideoFrames}
\title{GetVideoFrames}
\usage{
GetVideoFrames(video, start = 0, end = 99999, step = 1/30,
  savetodir = NA)
}
\arguments{
\item{video}{the eye tracking video that can be read by matlab VideoReader.}

\item{start}{the starting second of the extracted frames. Default is 0.}

\item{end}{the ending second of the extracted frames, default is the end time of video.}

\item{step}{the interval in seconds between frames, default is 1/30.}

\item{savetodir}{the output directory, default is the current directory.}
}
\value{
Returns the video frames extracted.
}
\description{
Break the video into individual frames
}
\examples{
\dontrun{
# Put the right version of Matlab that installed on your computer
options(matlab.path = "/Applications/MATLAB_R2016b.app/bin")
# Check having Matlab or not
library(matlabr)
have_matlab()
# Call function
GetVideoFrames(system.file("extdata", "recording.avi", package = "EyeTracking"),
 0, 2, 1/30)
}

}
\references{
See \url{https://www.mathworks.com/matlabcentral/fileexchange/46615-getvideoframes-vid--startt--endt--step--savetodir-}.
}
\author{
Chunyang Li < lichunyang1990@hotmail.com >
}
