% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OtherPlotsFunctions.R
\name{DrawEyeScatterplot}
\alias{DrawEyeScatterplot}
\title{Scatterplot}
\usage{
DrawEyeScatterplot(poster, locations, adjust = TRUE, ...)
}
\arguments{
\item{poster}{The location and name of the poster image file.}

\item{locations}{The location and name of the csv file of the eye tracking data coordinates.}

\item{adjust}{Adjust the difference in the coordinates from the results returned by Matlab. Default is TRUE.}

\item{...}{Arguments to be passed to methods, such as graphical parameters in \code{\link[graphics]{plot}}.}
}
\value{
Returns a scatterplot.
}
\description{
Creates a scatter plot showing the participant's focus points.
}
\examples{
data(locations_testing)
DrawEyeScatterplot(
poster = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
 locations = locations_testing[, 1:2])
}
\author{
Chunyang Li < lichunyang1990@hotmail.com >
}
