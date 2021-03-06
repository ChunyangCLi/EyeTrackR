% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OtherPlotsFunctions.R
\name{DrawEyeScanpathMap}
\alias{DrawEyeScanpathMap}
\title{DrawEyeScanpathMap}
\usage{
DrawEyeScanpathMap(poster, locations, fix.count = FALSE, scatter = FALSE,
  cex.scatter = 0.2, point.col = "red", dot.col = "blue",
  circle.size = 0.35, text.col = "white", lambda = 15, adjust = TRUE)
}
\arguments{
\item{poster}{The location and name of the poster image file.}

\item{locations}{The location and name of the csv file of the eye tracking data coordinates.}

\item{fix.count}{The number of fixations to plot. Default is FALSE, i.e., all the detected fixation points will be plotted.}

\item{scatter}{A logical variable indicating whether to show the scatter plot or not. Default is FALSE.}

\item{cex.scatter}{The size of dots for the scatter plot if it is TRUE. Default is 0.2.}

\item{point.col}{The color of the circles for the fixation points. Default is "red".}

\item{dot.col}{The color of dots for the scatter plot if it is TRUE. Default is "blue".}

\item{circle.size}{A standard size of the circle for the fixation points. Default is 0.35,}

\item{text.col}{The color of the numbers labelled in the circle. Default is "white".}

\item{lambda}{The threshold for fixation detection. The smaller lambda is, the more fixations it detects. Default is 15.}

\item{adjust}{Adjust the difference in the coordinates from the results returned by Matlab. Default is TRUE.}
}
\value{
Returns a scanpath plot.
}
\description{
Creates a scanpath indicating the length and the sequence of the participant's fixation points.
The numbers in the circles indicate the sequential order of the fixations and the radiuses of the circles indicate the duration of the fixations.
}
\details{
The fixation points are identified by \code{link[saccades]{detect.fixations}}.
}
\examples{
data(locations_testing)
data(scanpathdat.all)
scanpathdat = scanpathdat.all[[1]]
par(mar=c(0,0,0,0)+0.1)
DrawEyeScanpathMap(
poster = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
locations = locations_testing)
}
\references{
\url{https://cran.r-project.org/web/packages/saccades/saccades.pdf}.
}
\author{
Chunyang Li < lichunyang1990@hotmail.com >
}
