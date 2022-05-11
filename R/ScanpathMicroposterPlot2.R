
########################################
# Scanpath Microposter Plot
########################################
#' DrawEyeLSMPlot
#'
#'
#' Creates a linked scanpath microposter plot without predefined AOIs.
#'
#'
#' The fixation points are identified by \code{\link[saccades]{detect.fixations}}.
#'
#'
#' @param poster.loc The jpg image.
#' @param locations The directory and name of the csv file of the eye tracking data coordinates.
#' @param grayscale Whether to use grayscale image for the microposter or not. Default is TRUE.
#' @param adjust Adjust the difference in the coordinates from the results returned by Matlab. Default is TRUE.
#' @param AutomaticLayout Whether to use the automatic layout or not. Default is TRUE.
#' @param lambda The threshold for fixation detection. The smaller lambda is, the more fixations it detects. Default is 10.
#' @param frequency The frequency of the recording. Default is 30.
#' @param Layout Self defined layout: a vector contains how many observations in each panel. Default is NA.
#' @param MedianRow Whether have a separate row indicate the meidian or not, if possible. Default is FALSE.
#' @param partitioning Default is 2. See Symanzik, J. and Carr, D.B., 2008.
#' Interactive linked micromap plots for the display of geographically referenced statistical data.
#' In Handbook of data visualization (pp. 267-294). Springer Berlin Heidelberg.
#' @param sortby The name of the variable to sort by. Default is "visits".
#' Other options are "pupildat" and "visits".
#' @param decreasing Whether sort the data by a decreasing order or not. Default is TRUE.
#' @param panel.types A vector of the plot types for each columns.
#' The possible plot types are "poster", "legend", "dot", and "boxplot".
#' Default is c("poster", "dot", "boxplot").
#' @param panel.width A vector of the width for each column. Default is c(2.9, 2.9, 2.9).
#' @param hdColors A vector of colors for the observations for each panel. It is recommended to
#' provide at least six different colors. The median row (if exists) takes the last color listed.
#' Default is c(rev(RColorBrewer::brewer.pal(5, "Accent")), "#000000").
#' @param columns.att A list of panel attributes to be specified, including: col.num, cumulate, header, header.size,
#' point.size, text.size, panel.data, axis.ticks, point.size, and axis.labels.
#' @param col.num The number of colomn.
#' @param sequence.label A logical variable that determines whether to label the fixation sequences in the poster or not.
#' Default is TRUE.
#' @param label.size The size of the number that indicates the fixation sequences, if the sequence.label is TRUE.
#'  Default is 1.
#' @param circle.size The size of the circles that represent the fixation points. Default is 5.
#' @param line A logical variable indicates whether to connect the fixation points with lines or not.
#' @param header The title for the column.
# #' @param header.size The text size of the column title. Default is 0.7.
#' @param point.size The size of the point if there is any. Default is 0.96.
#' @param text.size The size of the text for the axis. Default is 0.7.
# #' @param text.font The font of the text for the axis. Default is 1.
#' @param panel.data The input data for the panel. e.g. "visits", "pupildat", and "visits.num".
#' @param axis.ticks A vector that indicate where to label the axis. If it is NA, the axis ticks are
#' automatly determined. Default is NA.
#' @param axis.labels The labels on the axis. If it is NA, the axis labels are automatly determined. Default is NA.
#' @param main.title the title of the plot. Default is "Eye Tracking: Looking at Poster Statistics".
#' @param title.cex the size of the title. Default is 1.08.
#' @return Returns a linked scanpath microposter plot without predefined AOIs.
#' @author Chunyang Li < lichunyang1990@hotmail.com >
#'
#' @export DrawEyeLSMPlot
#' @examples
#' data("locations_testing")
#'
# pdf("lsm_Plot2_example1.pdf", width = 6, height = 8.8)
#'
#' DrawEyeLSMPlot(poster.loc = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
#'           locations = locations_testing,
          # grayscale = TRUE,
          # adjust = TRUE,
          # AutomaticLayout = TRUE,
          # lambda = 10,
          # frequency = 30,
          # Layout = NA,
          # MedianRow = FALSE,
          # partitioning = 2,
          # sortby = "sequence",
          # decreasing = FALSE,
          # panel.types = c("poster", "dot", "boxplot"),
          # panel.width = c(2.9, 2.9, 2.9),
          # hdColors = c(rev(RColorBrewer::brewer.pal(5, "Accent")), "#000000"),
#'           columns.att = list(
#'             list(col.num = 1, sequence.label = TRUE, label.size = 1, circle.size = 5,
#'              line = TRUE, header = "Scanpath Microposters"),
#'             list(col.num = 2, panel.data = "visits", header = "Length of Visits (sec)",
#'              text.size = 0.7, point.size = 0.96, axis.ticks = NA, axis.labels = NA),
#'             list(col.num = 3, panel.data = "pupildat", header = "Pupil Radius in Pixels",
#'              text.size = 0.7, axis.ticks = NA, axis.labels = NA)
#'           )
          # main.title = "Eye Tracking: Looking at Scanpath Statistics",
          # title.cex = 1.08
#' )
# dev.off()
#'
#' # Example two: change the layout to self-defined layout and enabled the median row
#' # set lambda to 8 in order to have more fixations identified.
#'
#' data("locations_testing")
#'
# pdf("lsm_Plot2_example2.pdf", width = 6, height = 8.8)
#'
#' DrawEyeLSMPlot(poster.loc = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
#'           locations = locations_testing,
          # grayscale = TRUE,
          # adjust = TRUE,
          # AutomaticLayout = FALSE,
#'           lambda = 8,
          # frequency = 30,
#'           Layout = c(6, 5, 5, 1, 5, 5, 6),
          # MedianRow = TRUE,
          # partitioning = 2,
          # sortby = "sequence",
          # decreasing = FALSE,
          # panel.types = c("poster", "dot", "boxplot"),
          # panel.width = c(2.9, 2.9, 2.9),
#'           hdColors = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
#'           '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928', "#000000"),
#'           columns.att = list(
#'             list(col.num = 1, sequence.label = TRUE, label.size = 1, circle.size = 5,
#'              line = TRUE, header = "Scanpath Microposters"),
#'             list(col.num = 2, panel.data = "visits", header = "Length of Visits (sec)",
#'              text.size = 0.7, point.size = 0.96, axis.ticks = NA, axis.labels = NA),
#'             list(col.num = 3, panel.data = "pupildat", header = "Pupil Radius in Pixels",
#'              text.size = 0.7, axis.ticks = NA, axis.labels = NA)
#'           )
          # main.title = "Eye Tracking: Looking at Scanpath Statistics",
          # title.cex = 1.08
#' )
#'
# dev.off()
#'
#' # Example three: Sort by length of visits and remove the connecting line between fixations
#'
#' data("locations_testing")
#' library(RColorBrewer)
# pdf("lsm_Plot2_example3.pdf", width = 6, height = 8.8)
#'
#' DrawEyeLSMPlot(poster.loc = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
#'           locations = locations_testing,
          # grayscale = TRUE,
          # adjust = TRUE,
          # AutomaticLayout = TRUE,
          # lambda = 10,
          # frequency = 30,
          # Layout = NA,
          # MedianRow = TRUE,
          # partitioning = 2,
#'           sortby = "visits",
          # decreasing = FALSE,
          # panel.types = c("poster", "dot", "boxplot"),
          # panel.width = c(2.9, 2.9, 2.9),
          # hdColors = c(rev(RColorBrewer::brewer.pal(5, "Accent")), "#000000"),
#'           columns.att = list(
#'             list(col.num = 1, sequence.label = TRUE, label.size = 1, circle.size = 5,
#'              line = FALSE, header = "Scanpath Microposters"),
#'             list(col.num = 2, panel.data = "visits", header = "Length of Visits (sec)",
#'              text.size = 0.7, point.size = 0.96, axis.ticks = NA, axis.labels = NA),
#'             list(col.num = 3, panel.data = "pupildat", header = "Pupil Radius in Pixels",
#'              text.size = 0.7, axis.ticks = NA, axis.labels = NA)
#'           )
          # main.title = "Eye Tracking: Looking at Scanpath Statistics",
          # title.cex = 1.08
#' )
#'
# dev.off()






                 DrawEyeLSMPlot <- function(poster.loc,
                                       locations,
                                       grayscale = TRUE,
                                       adjust = FALSE,
                                       AutomaticLayout = TRUE,
                                       lambda = 10,
                                       frequency = 30,
                                       Layout = NA,
                                       MedianRow = FALSE,
                                       partitioning = 2,
                                       sortby = "sequence",
                                       decreasing = FALSE,
                                       panel.types = c("poster", "dot", "boxplot"),
                                       panel.width = c(2.9, 2.9, 2.9),
                                       hdColors = c(rev(RColorBrewer::brewer.pal(5, "Accent")), "#000000"),
                                       columns.att = list(
                                          list(col.num = 1, sequence.label = TRUE, label.size = 1, circle.size = 5, line = TRUE, header = "Scanpath Microposters"),
                                         list(col.num = 2, panel.data = "visits", header = "Length of Visits (sec)", text.size = 0.7, point.size = 0.96, axis.ticks = NA, axis.labels = NA),
                                         list(col.num = 3, panel.data = "pupildat", header = "Pupil Radius in Pixels", text.size = 0.7, axis.ticks = NA, axis.labels = NA)
                                       ),
                                       main.title = "Eye Tracking: Looking at Poster Statistics",
                                       title.cex = 1.08
)
                                      {

  columns = length(columns.att)
  if (length(panel.types) < columns){
     warning("Panel types are not completely specified.")
     }

  names(columns.att) = panel.types
  poster = imager::load.image(poster.loc)
  if (grayscale){
    poster = imager::grayscale(poster)
  }

  if (ncol(locations) == 3) {
    colnames(locations) = c("x", "y", "PupilSize")} else {
      colnames(locations) = c("x", "y")
    }
  locations = locations[(locations[, 1] != "NaN" & locations[, 2] != "NaN"), ]
  locations = locations[(locations[, 1] != 0 | locations[, 2] != 0), ]


  # To adjust the difference of axis between R and Matlab
  if(adjust){
  locations$y = imager::height(poster) - locations$y
  }

  loc2 = locations[, c("x", "y")]
  loc2$trial = rep(1, nrow(loc2))
  loc2$time = 1:nrow(loc2)
  my.fixations = saccades::detect.fixations(loc2, lambda)
  my.fixations = my.fixations[my.fixations$dur > 5, ]

  # Extract pupildat
  if (ncol(locations) == 3) {
  pupildat = sapply(1:nrow(my.fixations),function(x) NULL)

     for (i in 1:nrow(my.fixations)){

       temp = locations[my.fixations$start[i]:my.fixations$end[i], 3]
       pupildat[[i]] = temp[temp != -2000]

     }

  }
  posterdat = my.fixations
  posterdat$sequence = 1:nrow(posterdat)

  # sort data
  if (sortby == "pupildat"){
    unordered.median = unlist(lapply(pupildat, stats::median))
    ordered.median = order(unordered.median, decreasing = decreasing)
    pupildat = pupildat[ordered.median]
    posterdat = posterdat[ordered.median, ]
  } else if (sortby == "visits") {
    posterdat = posterdat[order(posterdat["dur"], decreasing = decreasing), ]
    pupildat = pupildat[order(posterdat["dur"], decreasing = decreasing)]
  }



  if (AutomaticLayout){
    rows = ArrangePanels(posterdat = posterdat, partitioning)[[3]]
    MedianRow = DetermineMedianRow(nrow(posterdat), partitioning)
  } else {
    rows = length(Layout)
    if (length(hdColors) < max(Layout)){
      hdColors = rep(hdColors, ceiling(max(Layout)/length(hdColors)))
      warning('There are too few colors provided.')
    }
  }

  if ((AutomaticLayout == FALSE) && MedianRow){
    if ((rows %% 2 == 0) || (Layout[ceiling(length(Layout)/2)] != 1)){
      MedianRow = FALSE
      warning("There is no median row.")
    }
  }


  if (MedianRow == FALSE){

    rowsize = rep(17.2/rows, rows)

  } else {

    rowsize = c(rep(17.2/rows, floor(rows/2)), 17.2/(rows*3.2), rep(17.2/rows, floor(rows/2)))

  }

  panels = panelLayout(nrow = rows, ncol = columns, topMargin = details$top, bottomMargin = details$bot, leftMargin = details$left,
                       rightMargin = details$right, rowSep = rep(0.01, rows + 1), rowSize = rowsize,
                       colSize = panel.width, colSep = rep(0.05, (columns + 1)))


  iBegin = ArrangePanels(posterdat = posterdat, partitioning, AutomaticLayout, Layout, MedianRow)[[1]]
  iEnd = ArrangePanels(posterdat = posterdat, partitioning, AutomaticLayout, Layout, MedianRow)[[2]]
  nGroups = length(iEnd)


  if (sum(panel.types == "poster") >= 1){

  sm.posters2(panel.col = columns.att$poster$col.num,
            sequence.label = columns.att$poster$sequence.label,
            label.size = columns.att$poster$label.size,
            circle.size = columns.att$poster$circle.size,
            line = columns.att$poster$line,
            panel.title = columns.att$poster$header,
            posterdat,
            poster,
            panels,
            nGroups,
            rows,
            MedianRow,
            hdColors,
            iBegin,
            iEnd)

  }


  if (sum(panel.types == "dot") >= 1){

    dots = which(names(columns.att) == "dot")

    for (i in 1:length(dots)){

      sm.dotplot2(var = columns.att[dots[i]]$dot$panel.data,
                 posterdat,
                 frequency,
                 panel.num = columns.att[dots[i]]$dot$col.num,
                 title = columns.att[dots[i]]$dot$header,
                 cex = columns.att[dots[i]]$dot$text.size,
                 dcex = columns.att[dots[i]]$dot$point.size,
                 axis.ticks = columns.att[dots[i]]$dot$axis.ticks,
                 axis.labels = columns.att[dots[i]]$dot$axis.labels,
                 panels,
                 MedianRow,
                 nGroups,
                 hdColors,
                 iBegin,
                 iEnd)
    }
  }



  if (sum(panel.types == "boxplot") >= 1){

    bplot = which(names(columns.att) == "boxplot")

    for (i in 1:length(bplot)){
      sm.boxplot2(dat = columns.att[bplot[i]]$boxplot$panel.data,
                 pupildat,
                 panel.num = columns.att[bplot[i]]$boxplot$col.num,
                 text.size = columns.att[bplot[i]]$boxplot$text.size,
                 axis.ticks = columns.att[bplot[i]]$boxplot$axis.ticks,
                 axis.labels = columns.att[bplot[i]]$boxplot$axis.labels,
                 title = columns.att[bplot[i]]$boxplot$header,
                 panels,
                 MedianRow,
                 nGroups,
                 hdColors,
                 iBegin,
                 iEnd)

    }
  }


  # 11 Add Title and Legend _____________________________________________
  ## Run
  panelSelect(panels, margin = "top")
  invisible(panelScale())
  graphics::text(0.5, 0.75, labels = main.title, cex = details$tCex)

  panelSelect(panels, margin = "bottom")
  invisible(panelScale(inches = TRUE))
  ## End

}


