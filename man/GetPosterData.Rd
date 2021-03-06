% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataSummary.R
\name{GetPosterData}
\alias{GetPosterData}
\title{GetPosterData}
\usage{
GetPosterData(posterVBorders = posterVBorders, locations = locations,
  threshold = 3, frequency = 30, return.blank = TRUE, adjust = TRUE)
}
\arguments{
\item{posterVBorders}{The output csv file from the \code{\link{DrawAOIs}} function.}

\item{locations}{The location and name of the csv file of the eye tracking data coordinates.}

\item{threshold}{Defines for at least how many images the coordinates fall into a certain AOI
to be considered as a visit. Default is 3.}

\item{frequency}{The frequency of the eye tracker. Default is 30.}

\item{return.blank}{A logical variable determines whether to return the statistics of
when the participant look at other areas than the defined AOIs. Default is TRUE.}

\item{adjust}{Adjust the difference in the coordinates from the results returned by Matlab. Default is TRUE.}
}
\value{
A list of two or three data frames and lists with summarized statistics of viewing data according to the AOIs drawn.
}
\description{
Summarize poster viewing data according to the AOIs drawn
}
\details{
The fist element of the list is a data frame contains for how long and how many times participant looked at each AOIs ;
The second element of the list is a list of eye movement speed in pixels for the visit at each AOIs;
The third element of the list is a list of pupil radiuses for each visit at the coorespoinding AOI, if the pupil radius is provided in the location csv file.
}
\examples{
posterdat.all = GetPosterData(posterVBorders = AOIName,
                            locations = locations_testing)
}
\author{
Chunyang Li < lichunyang1990@hotmail.com >
}
