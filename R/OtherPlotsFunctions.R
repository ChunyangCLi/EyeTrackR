
### Reproduce the basic plot types

#' DrawEyeDotplot
#'
#'
#' Extract the coordinates of the crosshair in terms of the poster from the video frames
#'
#' @param posterdat the directory of the video frames
#' @param decreasing TRUE for sorting the data in descending order. Default is TRUE.
#' @param xlab Label for x axis. Default is 'Area of Interest'.
#' @param ylab Label for y axis. Default is 'Length of Visits (sec)'.
#' @param ... Arguments to be passed to methods, such as graphical parameters in \code{\link[graphics]{plot}}.
#' @return Returns a dot plot.
#' @export DrawEyeDotplot
#' @examples
#' data(posterdat.all)
#' posterdat = posterdat.all[[1]]
#' DrawEyeDotplot(posterdat = posterdat)
# decreasing = TRUE,
# xlab = "Area of Interest",
# ylab = "Length of Visits (sec)")
#'

#' @author Chunyang Li < lichunyang1990@hotmail.com >
#' @references  See \url{https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/plot.html}.
#' @seealso \code{\link{GetPosterData}}

# Dot Plots

DrawEyeDotplot <- function( posterdat = posterdat,
                     decreasing = TRUE,
                     xlab = "Area of Interest",
                     ylab = "Length of Visits (sec)",
                     ...) {
posterdat = posterdat[order(posterdat[, 2], decreasing = decreasing), ]
graphics::plot(posterdat[, 2], pch = 19, type = "p", xaxt = "n", cex = 1.4,
     xlab = xlab, ylab = ylab, ...)
graphics::axis(1, at=1:length(posterdat[, 2]), labels =F)
graphics::text(x = 1:length(posterdat[, 2]), graphics::par("usr")[3] - 0.5 , labels = posterdat[, 1] , srt = 45, xpd = TRUE, cex = 1)
}


#' DrawEyeBarplot
#'
#'
#' Creates a bar plot of how long one is looking at each AOIs.
#'
#' @param posterdat Summarized eye tracking data with function GetPosterData.
#' @param decreasing TRUE for sorting the data in descending order. Default is TRUE.
#' @param xlab Label for x axis. Default is empty.
#' @param ylab Label for y axis. Default is 'Length of Visits (sec)'.
#' @param ... Arguments to be passed to methods, such as graphical parameters in \code{\link[graphics]{barplot}}.
#' @return Returns a bar plot.
#' @author Chunyang Li < lichunyang1990@hotmail.com >
#' @references  See \url{https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/barplot.html}.
#' @seealso \code{\link{GetPosterData}}
#' @export DrawEyeBarplot
#' @examples
#' data(posterdat.all)
#' posterdat = posterdat.all[[1]]
#' DrawEyeBarplot(posterdat = posterdat)



# Bar chart
DrawEyeBarplot <- function(posterdat = posterdat,
                    decreasing = TRUE,
                    xlab = "",
                    ylab = "Length of Visits (sec)",
                    ...){
graphics::barplot(as.numeric(posterdat[order(posterdat[, 2], decreasing = decreasing), 2]),
        xlab="", ylab = ylab,
        names.arg = posterdat[, 1], las = 2, cex.names = 1.2, ...)

}


# Box plot

#' DrawEyeBoxplot
#'
#' Creates a box plot of participant's pupil radiuses while looking at each AOIs.
#'
#' @param pupildat Summarized pupil data with function GetPosterData.
#' @param decreasing TRUE for sorting the data in descending order. Default is TRUE.
#' @param ... Arguments to be passed to methods, such as graphical parameters in \code{\link[graphics]{boxplot}}.

#' @return Returns a box plot of participant's pupil radiuses while looking at each AOIs.
#' @author Chunyang Li < lichunyang1990@hotmail.com >
#' @references  See \url{https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/boxplot.html}.
#' @export DrawEyeBoxplot
#' @examples
#' \dontrun{
#' data(posterdat.all)
#' pupildat = posterdat.all[[3]]
#' DrawEyeBoxplot(pupildat = pupildat)
#' }


DrawEyeBoxplot <- function(pupildat = pupildat, decreasing = TRUE, ...){

unordered.median = unlist(lapply(pupildat, stats::median))
ordered.median = order(unordered.median, decreasing = decreasing)
pupildat2 = pupildat[ordered.median]
pupildat.median = lapply(pupildat, function(x) stats::median(x))
graphics::par(cex.lab = 1.2)
graphics::par(cex.axis = 1.2)
graphics::boxplot(pupildat2, las = 2, ylim = c(40, 70), ...)
graphics::title("", ylab = "Pupil Radius in Eye Image Pixels")

}


#' DrawEyeHeatmap
#'
#' Creates a heat map with Gaussian kernel function showing where the participant focus most.
#'
#' @param poster The location and name of the poster image file.
#' @param locations The directory and name of the csv file of the eye tracking data coordinates.
#' @param bandwidth The bandwidth of the heatmap. The default value is c(60, 60).
#' @param main The title. Default is empty.
#' @param adjust Adjust the difference in the coordinates from the results returned by Matlab. Default is TRUE.
#' @return Returns a heatmap.
#' @export DrawEyeHeatmap
#' @author Chunyang Li < lichunyang1990@hotmail.com >
#' @examples
#' data(locations_testing)
#' graphics::par(mar = c(0, 0, 0, 0) + 0.1)
#' DrawEyeHeatmap(poster = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
#' locations = locations_testing)


### heap map

DrawEyeHeatmap <- function(poster, locations, bandwidth = c(60, 60), main = "", adjust = TRUE){
  Image = jpeg::readJPEG(poster)
  graphics::plot(c(0, ncol(Image)), c(0, nrow(Image)), type = "n", xlab = "", ylab = "", axes = FALSE)
  graphics::axis(1, labels = FALSE, tick = FALSE)
  graphics::axis(2, labels = FALSE, tick = FALSE)
  graphics::rasterImage(Image, 0, 0, ncol(Image), nrow(Image))
  graphics::rect(0, 0, ncol(Image), nrow(Image))
  locations = locations[(locations[, 1] != "NaN" & locations[, 2] != "NaN"), ]
  locations = locations[(locations[, 1] != 0 | locations[, 2] != 0), ]

  # To adjust the difference of axis between R and Matlab
  if (adjust){
  locations[, 2] = nrow(Image) - locations[, 2]
  }

  cols = c(grDevices::colorRampPalette(c("navyblue", "dodgerblue3", "gold", "orange2", "red4"), space = "rgb", interpolate = "linear")(100))
  cols = scales::alpha(cols, 0.4)
  tt = MASS::kde2d(x = locations[, 1], y = locations[, 2], h = bandwidth, n = 800, lims = c(0, ncol(Image), 1, nrow(Image)))
  tt.raster = raster::raster(tt)
  raster::plot(tt.raster, col = cols, main = main, add = TRUE, legend = FALSE)

}

#' Scatterplot
#'
#' Creates a scatter plot showing the participant's focus points.
#'
#' @param poster The location and name of the poster image file.
#' @param locations The location and name of the csv file of the eye tracking data coordinates.
#' @param adjust Adjust the difference in the coordinates from the results returned by Matlab. Default is TRUE.
#' @param ... Arguments to be passed to methods, such as graphical parameters in \code{\link[graphics]{plot}}.
#' @return Returns a scatterplot.
#' @author Chunyang Li < lichunyang1990@hotmail.com >
#' @export DrawEyeScatterplot
#' @examples
#' data(locations_testing)
#' DrawEyeScatterplot(
#' poster = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
#'  locations = locations_testing[, 1:2])

### Scatterplot

DrawEyeScatterplot <- function(poster, locations, adjust = TRUE, ...){

  Image = jpeg::readJPEG(poster)
  graphics::par(mar = c(0, 0, 0, 0) + 0.1)
  graphics::plot(c(0, ncol(Image)), c(0, nrow(Image)), type = "n", xlab = "", ylab = "", axes = FALSE, ...)
  graphics::axis(1, labels = FALSE, tick = FALSE)
  graphics::axis(2, labels = FALSE, tick = FALSE)
  graphics::rasterImage(Image, 0, 0, ncol(Image), nrow(Image))
  graphics::rect(0, 0, ncol(Image), nrow(Image))
  locations = locations[(locations[, 1] != "NaN" & locations[, 2] != "NaN"), ]
  locations = locations[(locations[, 1] != 0 | locations[, 2] != 0), ]
  # To adjust the difference of axis between R and Matlab
  if (adjust) {
  locations[, 2] = nrow(Image) - locations[, 2]
  }
  graphics::points(locations[, 1:2])

}



#' DrawEyeAOITimelines
#'
#' Creates an AOI timeline plot indicating the length and the sequence of how the participant looks at each AOIs.
#'
#' @param scanpathdat Summarized scan path eye tracking data with function get.scanpathdat.
#' @param xlab Label for x axis. Default is "AOIs".
#' @param ylab Label for y axis. Default is "Time (second)".
#' @param main.title The title. Default is "Poster AOI Timelines".
#' @param label.cex The size of the labels on the x axis. Default is 1.05.
#' @return Returns DrawEyeAOITimelines plot.
#' @author Chunyang Li < lichunyang1990@hotmail.com >
#' @export DrawEyeAOITimelines
#' @examples
#' data(scanpathdat.all)
#' scanpathdat = scanpathdat.all[[1]]
#' DrawEyeAOITimelines(scanpathdat = scanpathdat)
#'


### DrawEyeAOITimelines

DrawEyeAOITimelines <- function(scanpathdat,
                         xlab = 'AOIs',
                         ylab = 'Time (second)',
                         main.title = "Poster AOI Timelines",
                         label.cex = 1.05){

cumvisits = base::cumsum(scanpathdat$visits)
unisec = base::unique(scanpathdat$section)
plotdat = matrix(rep(0, base::nrow(scanpathdat)*2), nrow = base::nrow(scanpathdat), ncol = 2)
for (i in 1:base::nrow(scanpathdat)){
  plotdat[i, 1] = base::which(unisec == scanpathdat$section[i])
  plotdat[i, 2] = cumvisits[i]
}

# Make a new dataset to plot
plotdat2 = base::matrix(rep(0, base::nrow(scanpathdat)*4), nrow = base::nrow(scanpathdat)*2, ncol = 2)
plotdat2[2, ] = plotdat[1, ]
plotdat2[1, ] = c(1, 0)
for (i in 2:base::nrow(scanpathdat)){
  plotdat2[((i - 1)*2 + 2), ] = plotdat[i, ]
  plotdat2[((i - 1)*2 + 1), 1] = plotdat[i, 1]
  plotdat2[((i - 1)*2 + 1), 2] = plotdat[(i - 1), 2]
}

graphics::plot(plotdat2, type = "l", xaxt = "n", xlab = xlab, ylab = ylab)
graphics::axis(1, at = 1:length(unisec), labels = FALSE)
graphics::abline(v = 1:length(unisec), col = scales::alpha("gray", 0.5), lty = 5)
graphics::text(x = 1:length(unisec), graphics::par("usr")[3] - 3 , labels = unisec , srt = 45, xpd = TRUE, cex = label.cex)
graphics::title(main.title)

}


#' DrawEyeScanpathMap
#'
#' Creates a scanpath indicating the length and the sequence of the participant's fixation points.
#' The numbers in the circles indicate the sequential order of the fixations and the radiuses of the circles indicate the duration of the fixations.
#'
#' The fixation points are identified by \code{link[saccades]{detect.fixations}}.
#'
#' @param poster The location and name of the poster image file.
#' @param locations The location and name of the csv file of the eye tracking data coordinates.
#' @param fix.count The number of fixations to plot. Default is FALSE, i.e., all the detected fixation points will be plotted.
#' @param scatter A logical variable indicating whether to show the scatter plot or not. Default is FALSE.
#' @param cex.scatter The size of dots for the scatter plot if it is TRUE. Default is 0.2.
#' @param point.col The color of the circles for the fixation points. Default is "red".
#' @param dot.col The color of dots for the scatter plot if it is TRUE. Default is "blue".
#' @param circle.size A standard size of the circle for the fixation points. Default is 0.35,
#' @param text.col The color of the numbers labelled in the circle. Default is "white".
#' @param lambda The threshold for fixation detection. The smaller lambda is, the more fixations it detects. Default is 15.
#' @param adjust Adjust the difference in the coordinates from the results returned by Matlab. Default is TRUE.
#' @return Returns a scanpath plot.
#' @author Chunyang Li < lichunyang1990@hotmail.com >
#' @references \url{https://cran.r-project.org/web/packages/saccades/saccades.pdf}.
#' @export DrawEyeScanpathMap
#' @examples
#' data(locations_testing)
#' data(scanpathdat.all)
#' scanpathdat = scanpathdat.all[[1]]
#' par(mar=c(0,0,0,0)+0.1)
#' DrawEyeScanpathMap(
#' poster = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
#' locations = locations_testing)
# fix.count = FALSE,
# scatter = FALSE,
# cex.scatter = 0.2,
# point.col = "red",
# dot.col = "blue",
# circle.size = 0.35,
# text.col = "white",
# lambda = 15,
# adjust = TRUE)

### Scanpath Visualization

DrawEyeScanpathMap <- function(poster,
                          locations,
                          fix.count = FALSE,
                          scatter = FALSE,
                          cex.scatter = 0.2,
                          point.col = "red",
                          dot.col = "blue",
                          circle.size = 0.35,
                          text.col = "white",
                          lambda = 15,
                          adjust = TRUE) {

  Image = imager::load.image(poster)

  if (ncol(locations) == 3) {
    colnames(locations) = c("x", "y", "PupilSize")} else {
      colnames(locations) = c("x", "y")
    }
  locations = locations[(locations[, 1] != "NaN" & locations[, 2] != "NaN"), ]
  locations = locations[(locations[, 1] != 0 | locations[, 2] != 0), ]


  # To adjust the difference of axis between R and Matlab
  if (adjust) {
    locations$y = imager::height(Image) - locations$y
  }

  loc2 = locations[, c("x", "y")]
  loc2$trial = rep(1, nrow(loc2))
  loc2$time = 1:nrow(loc2)

  graphics::plot(c(0, imager::width(Image)), c(0, imager::height(Image)), type = "n", xlab = "", ylab = "", axes = FALSE)
  graphics::axis(1, labels = FALSE, tick = FALSE)
  graphics::axis(2, labels = FALSE, tick = FALSE)
  graphics::rasterImage(Image, 0, 0, imager::width(Image), imager::height(Image))
  graphics::rect(0, 0, imager::width(Image), imager::height(Image))
  my.data2 = loc2

  my.data2$time <- as.numeric(my.data2$time)
  my.fixations <- saccades::detect.fixations(my.data2, lambda)
  my.fixations <- my.fixations[my.fixations$dur > 5, ]

  if (fix.count) {
    biggest.data <- order(my.fixations$dur, decreasing = TRUE)[1:fix.count]
  } else {
    biggest.data <- 1:nrow(my.fixations)
  }

  radius <- sqrt(my.fixations$dur/pi)
  graphics::symbols(my.fixations$x[biggest.data], my.fixations$y[sort(biggest.data)], circles = radius,
          inches = circle.size, fg = scales::alpha(point.col, 0.6), bg = scales::alpha(point.col, 0.6), add = TRUE)

  graphics::lines(my.fixations$x[biggest.data],
        my.fixations$y[sort(biggest.data)],
        pch = 16,
        col = point.col)
  graphics::text(my.fixations$x[biggest.data],
       my.fixations$y[sort(biggest.data)],
       labels = 1:length(biggest.data),
       col = text.col)
  if (scatter) {
    graphics::points(my.data2$x, my.data2$y, pch = 19, cex = cex.scatter, col = dot.col)
  }
}

