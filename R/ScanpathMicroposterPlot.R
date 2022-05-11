
########################################
# Scanpath Microposter Plot
########################################

#' DrawEyeLTMPlot
#'
#'
#' Creates a linked scanpath microposter plot.
#'
#'
#' @param poster.loc The jpg image.
#' @param grayscale Whether to use grayscale image for the microposter or not. Default is TRUE.
#' @param AutomaticLayout Whether to use the automatic layout or not. Default is TRUE.
#' @param data A list of summarized data by \code{\link{GetPosterData}}.
#' @param posterVBorders The Area of Interest border information data created by \code{\link{DrawAOIs}}.
#' @param Layout Self defined layout: a vector contains how many observations in each panel. Default is NA.
#' @param MedianRow Whether have a separate row indicate the meidian or not, if possible. Default is TRUE.
#' @param partitioning Default is 1. See Symanzik, J. and Carr, D.B., 2008.
#' Interactive linked micromap plots for the display of geographically referenced statistical data.
#' In Handbook of data visualization (pp. 267-294). Springer Berlin Heidelberg.
#' @param sortby The name of the variable to sort by. Default is "sequence".
#' Other options are "pupildat", "speed", and "visits".
#' @param decreasing Whether sort the data by a decreasing order or not. Default is TRUE.
#' @param panel.types A vector of the plot types for each columns.
#' The possible plot types are "poster", "legend", "dot", and "boxplot".
#' Default is c("poster", "legend", "dot", "dot", "boxplot").
#' @param panel.width A vector of the width for each column. Default is c(2.9, 2.7, 3, 3, 3).
#' @param hdColors A vector of colors for the observations for each panel. It is recommended to
#' provide at least six different colors. #' Default is c(rev(RColorBrewer::brewer.pal(5, "Accent")), "#000000").
#' @param colorby Two options: "AOIs" or "panels". Default is "AOIs".
#' Determines either to color the AOIs by AOIs (every AOI has
#'  a different color if there are plenty of colors provided) or by panels
#'  (the color is arranged by different panels and get recycled for each panel).
#' @param columns.att A list of panel attributes to be specified, including: col.num, header, header.size,
#' point.size, text.size, panel.data, axis.ticks, point.size, and axis.labels.
#' @param col.num The number of colomn.
#' @param header The title for the column.
#' @param header.size The text size of the column title. Default is 0.7.
#' @param point.size The size of the point if there is any. Default is 0.96.
#' @param text.size The size of the text for the axis. Default is 0.7.
#' @param text.font The font of the text for the axis. Default is 1.
#' @param panel.data The input data for the panel. e.g. "visits", "pupildat", and "visits.num".
#' @param axis.ticks A vector that indicate where to label the axis. If it is NA, the axis ticks are
#' automatly determined. Default is NA.
#' @param axis.labels The labels on the axis. If it is NA, the axis labels are automatly determined. Default is NA.
#' @param main.title the title of the plot. Default is "Linked Timeline Microposter Plot".
#' @param title.cex the size of the title. Default is 1.08.
#' @return Returns a linked scanpath microposter plot.
#' @author Chunyang Li < lichunyang1990@hotmail.com >
#'
#' @export DrawEyeLTMPlot
#' @examples
#' ### Example one
#' data("AOIName", "scanpathdat.all")
#' # Remove the outliers of the speed
#' speed = scanpathdat.all[[2]]
#' scanpathdat.all[[2]] = lapply(speed, function(x) x[x < 100])
# pdf("ScanpathPoster_Plot.pdf", width = 8.5, height = 10)
#' DrawEyeLTMPlot(poster.loc = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
         # grayscale = TRUE,
         # AutomaticLayout = TRUE,
#'          data = scanpathdat.all,
#'          posterVBorders = AOIName,
         # Layout = NA,
         # MedianRow = TRUE,
#'          partitioning = 2,
         # sortby = "sequence",
#'          decreasing = FALSE,
#'          panel.types = c("poster", "legend", "dot", "boxplot", "boxplot"),
#'          panel.width = c(2, 2, 2.5, 2.5, 2.5),
#'          hdColors = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
#'          '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'),
         # colorby = "AOIs",
#'          columns.att = list(
#'          list(col.num = 1, sequence.label = TRUE, label.size = 1),
#'          list(col.num = 2, header = "Areas of Interest", header.size = 0.7,
#'          point.size = 0.96, text.size = 0.7, text.font = 1),
#'          list(col.num = 3, panel.data = "visits", header = "Length of Visits (sec)",
#'          text.size = 0.7, point.size = 0.96, axis.ticks = NA, axis.labels = NA),
#'          list(col.num = 4, panel.data = "pupildat", header = "Pupil Radius in Pixels",
#'          text.size = 0.7, axis.ticks = NA, axis.labels = NA),
#'          list(col.num = 5, panel.data = "speed", header = "Eye Movement Speed in Pixels",
#'          text.size = 0.7, axis.ticks = NA, axis.labels = NA)
#'                             ),
#'          main.title = "Linked Timeline Microposter Plot"
         # title.cex = 1.08
#' )
# dev.off()
#'
#'
#' # Example two: only two statistical panels; arrange panels by color;
#' # rearrange the panels
#'
#' data("AOIName", "scanpathdat.all")
#' # Remove the outliers of the speed
#' speed = scanpathdat.all[[2]]
#' scanpathdat.all[[2]] = lapply(speed, function(x) x[x < 100])
#'
#' library(RColorBrewer)
#'
# pdf("ScanpathPoster_Plot.pdf", width = 8.5, height = 10)
#' DrawEyeLTMPlot(poster.loc = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
         # grayscale = TRUE,
         # AutomaticLayout = TRUE,
#'          data = scanpathdat.all,
#'          posterVBorders = AOIName,
         # Layout = NA,
         # MedianRow = TRUE,
#'          partitioning = 2,
         # sortby = "sequence",
#'          decreasing = FALSE,
#'          panel.types = c("poster", "legend", "dot", "boxplot"),
#'          panel.width = c(2.5, 2.5, 2, 2),
#'          hdColors = rev(RColorBrewer::brewer.pal(5, "Accent")),
#'          colorby = "panels",
#'          columns.att = list(
#'            list(col.num = 4, sequence.label = TRUE, label.size = 1),
#'            list(col.num = 3, header = "Areas of Interest", header.size = 0.7,
#'            point.size = 0.96, text.size = 0.7, text.font = 1),
#'            list(col.num = 2, panel.data = "visits", header = "Length of Visits (sec)",
#'            text.size = 0.7, point.size = 0.96, axis.ticks = NA, axis.labels = NA),
#'            list(col.num = 1, panel.data = "pupildat", header = "Pupil Radius in Pixels",
#'            text.size = 0.7, axis.ticks = NA, axis.labels = NA)
#'          )
         # main.title = "Linked Timeline Microposter Plot",
         # title.cex = 1.08
#' )
#'
# dev.off()
#'
#'
#' # Example 3: rearrange panels and sort by pupil radius
#'
#' data("AOIName", "scanpathdat.all")
#' # Remove the outliers of the speed
#' speed = scanpathdat.all[[2]]
#' scanpathdat.all[[2]] = lapply(speed, function(x) x[x < 100])
#'
#' library(RColorBrewer)
#'
# pdf("ScanpathPoster_Plot.pdf", width = 8.5, height = 10)
#' DrawEyeLTMPlot(poster.loc = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
         # grayscale = TRUE,
         # AutomaticLayout = FALSE,
#'          data = scanpathdat.all,
#'          posterVBorders = AOIName,
#'          Layout = c(5, 3, 3, 5),
         # MedianRow = TRUE,
#'          partitioning = 2,
#'          sortby = "pupildat",
#'          decreasing = TRUE,
#'          panel.types = c("poster", "legend", "dot", "boxplot", "boxplot"),
#'          panel.width = c(2, 2, 2.5, 2.5, 2.5),
#'          hdColors = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
#'          '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'),
         # colorby = "AOIs",
#'          columns.att = list(
#'            list(col.num = 1, sequence.label = TRUE, label.size = 1),
#'            list(col.num = 2, header = "Areas of Interest", header.size = 0.7,
#'            point.size = 0.96, text.size = 0.7, text.font = 1),
#'            list(col.num = 3, panel.data = "visits", header = "Length of Visits (sec)",
#'             text.size = 0.7, point.size = 0.96, axis.ticks = NA, axis.labels = NA),
#'            list(col.num = 5, panel.data = "speed", header = "Eye Movement Speed in Pixels",
#'             text.size = 0.7, axis.ticks = NA, axis.labels = NA),
#'            list(col.num = 4, panel.data = "pupildat", header = "Pupil Radius in Pixels",
#'            text.size = 0.7, axis.ticks = NA, axis.labels = NA)
#'          )
         # main.title = "Linked Timeline Microposter Plot",
         # title.cex = 1.08
#' )
#'
# dev.off()


                 DrawEyeLTMPlot <- function(poster.loc,
                                      grayscale = TRUE,
                                      AutomaticLayout = TRUE,
                                      data,
                                      posterVBorders,
                                      Layout = NA,
                                      MedianRow = TRUE,
                                      partitioning = 1,
                                      sortby = "sequence",
                                      decreasing = TRUE,
                                      colorby = "AOIs",
                                      panel.types = c("poster", "legend", "dot", "boxplot", "boxplot"),
                                      panel.width = c(2.9, 2.9, 2.9, 2.9, 2.9),
                                      hdColors = c(rev(RColorBrewer::brewer.pal(5, "Accent")), "#000000"),
                                      columns.att = list(
                                        list(col.num = 1, sequence.label = TRUE, label.size = 1),
                                        list(col.num = 2, header = "Areas of Interest", header.size = 0.7, point.size = 0.96, text.size = 0.7, text.font = 1),
                                        list(col.num = 3, panel.data = "visits", header = "Length of Visits (sec)", text.size = 0.7, point.size = 0.96, axis.ticks = NA, axis.labels = NA),
                                        list(col.num = 5, panel.data = "speed", header = "Eye Movement Speed in Pixels", text.size = 0.7, axis.ticks = NA, axis.labels = NA),
                                        list(col.num = 4, panel.data = "pupildat", header = "Pupil Radius in Pixels", text.size = 0.7, axis.ticks = NA, axis.labels = NA)
                                      ),
                                      main.title = "Linked Timeline Microposter Plot",
                                      title.cex = 1.08
){

  columns = length(columns.att)
   if (length(panel.types) < columns){
     warning("Panel types are not completely specified.")
   }

  posterdat = data[[1]]
  speed = data[[2]]

  if(length(data) == 4){
    pupildat = data[[4]]
  }

  names(columns.att) = panel.types
  poster = imager::load.image(poster.loc)
  if (grayscale){
    poster = imager::grayscale(poster)
  }

  # Add borders for Blank
  # posterVBorders$y = imager::height(poster) - posterVBorders$y
  stateVBorders = posterVBorders

  nationVBorders = matrix(c(0, 0, imager::width(poster), 0, imager::width(poster), imager::height(poster), 0, imager::height(poster), 0, 0),
                          nrow = 5, ncol = 2, byrow = TRUE)
  nationVBorders = as.data.frame(nationVBorders)
  colnames(nationVBorders) = c("x", "y")
  sec = length(unique(stateVBorders$section))

  if (colorby == "AOIs" && length(hdColors) < sec){
    hdColors = rep(hdColors, ceiling(sec/length(hdColors)))
    warning("There are more AOIs than colors provided")
  }

  # sort data
  if (sortby == "pupildat"){
    unordered.median = unlist(lapply(pupildat, stats::median))
    ordered.median = order(unordered.median, decreasing = decreasing)
    pupildat = pupildat[ordered.median]
    posterdat = posterdat[ordered.median, ]
    speed = speed[ordered.median]
    data[[1]] = posterdat
    data[[4]] = pupildat
    data[[2]] = speed
  } else if (sortby == "speed"){
    unordered.median = unlist(lapply(speed, stats::median))
    ordered.median = order(unordered.median, decreasing = decreasing)
    speed = speed[ordered.median]
    posterdat = posterdat[ordered.median, ]
    data[[1]] = posterdat
    data[[4]] = pupildat
    data[[2]] = speed
  } else {
    posterdat = posterdat[order(posterdat[, sortby], decreasing = decreasing), ]
    data[[1]] = posterdat
    data[[4]] = pupildat
    data[[2]] = speed
  }


  ##################

  rows = ArrangePanels(posterdat = posterdat, partitioning)[[3]]
  MedianRow = DetermineMedianRow(nrow(posterdat), partitioning)

  if (AutomaticLayout){
    rows = ArrangePanels(posterdat = posterdat, partitioning)[[3]]
    MedianRow = DetermineMedianRow(nrow(posterdat), partitioning)
  } else {
    rows = length(Layout)
    if ((colorby == "panels") && (length(hdColors) < max(Layout))){
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


  ## End

  # 7.  Plot maps_________________________________________________________
  if (sum(panel.types == "poster") >= 1){

    sm.posters(panel.col = columns.att$poster$col.num,
                 posterdat = posterdat,
                 sequence.label = columns.att$poster$sequence.label,
                 label.size = columns.att$poster$label.size,
                 stateVBorders,
                 nationVBorders,
                 poster,
                 panels,
                 colorby,
                 nGroups,
                 rows,
                 MedianRow,
                 hdColors,
                 iBegin,
                 iEnd)

  }

  ## End

  # 8. Plot labels____________________________________________________________
  if (sum(panel.types == "legend") >= 1){

    sm.labels(panel.col = columns.att$legend$col.num,
             title = columns.att$legend$header,
             header.size = columns.att$legend$header.size,
             cex = columns.att$legend$text.size,
             dcex = columns.att$legend$point.size,
             font = columns.att$legend$text.font,
             posterdat = posterdat,
             stateVBorders,
             nationVBorders,
             panels,
             colorby,
             nGroups,
             MedianRow,
             hdColors,
             iBegin,
             iEnd)

  }

  # 9 Plot length of visits___________________________________

  if (sum(panel.types == "dot") >= 1){

    dots = which(panel.types == "dot")

    for (i in 1:length(dots)){

      sm.dotplot(var = columns.att[dots[i]]$dot$panel.data,
                posterdat,
                panel.num = columns.att[dots[i]]$dot$col.num,
                title = columns.att[dots[i]]$dot$header,
                cex = columns.att[dots[i]]$dot$text.size,
                dcex = columns.att[dots[i]]$dot$point.size,
                axis.ticks = columns.att[dots[i]]$dot$axis.ticks,
                axis.labels = columns.att[dots[i]]$dot$axis.labels,
                panels,
                colorby,
                nGroups,
                hdColors,
                iBegin,
                iEnd)
    }
  }


  # 10 b.  Plot Pupil Size___________________________________________________

  if (sum(panel.types == "boxplot") >= 1){

    bplot = which(panel.types == "boxplot")

    for (i in 1:length(bplot)){
      sm.boxplot(dat = columns.att[bplot[i]]$boxplot$panel.data,
                posterdat = data,
                panel.num = columns.att[bplot[i]]$boxplot$col.num,
                text.size = columns.att[bplot[i]]$boxplot$text.size,
                axis.ticks = columns.att[bplot[i]]$boxplot$axis.ticks,
                axis.labels = columns.att[bplot[i]]$boxplot$axis.labels,
                panels,
                colorby,
                title = columns.att[bplot[i]]$boxplot$header,
                nGroups,
                hdColors,
                iBegin,
                iEnd)
    }
  }

  # 11 Add Title and Legend _____________________________________________
  panelSelect(panels, margin = "top")
  invisible(panelScale())
  graphics::text(0.5, 0.75, labels = main.title, cex = details$tCex)

  panelSelect(panels, margin = "bottom")
  invisible(panelScale(inches = TRUE))


}

