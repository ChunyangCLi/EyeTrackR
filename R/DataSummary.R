
##########################################################################
# Summarize data according to the AOIs drawn
##########################################################################

#' GetAOITimelineData
#'
#' Summarize scanpath data according to the AOIs drawn
#'
#' The fist element of the list is a data frame contains the length of visits for each visit and the temporal sequence of the visits;
#' The second element of the list is a list of eye movement speed in pixels for the visit at each AOIs;
#' The third element of the list is the summary of the length and times the participant is looking at the areas not included in any AOIs;
#' The fourth element of the list is a list of pupil radiuses for each visit at the coorespoinding AOI, if the pupil radius is provided in the location csv file.
#'
#'
#'@param posterVBorders The output csv file from the \code{\link{DrawAOIs}} function.
#'@param locations The location and name of the csv file of the eye tracking data coordinates.
#'@param threshold Defines for at least how many images the coordinates fall into a certain AOI to be considered as a visit.
#'@param frequency The frequency of the eye tracker.
#'@param adjust Adjust the difference in the coordinates from the results returned by Matlab. Default is TRUE.
#'@return A list of three or four data frames and lists with summarized statistics of scanpath data according to the AOIs drawn.
#'@export GetAOITimelineData
#'@author Chunyang Li < lichunyang1990@hotmail.com >
#'@examples
#'scanpathdat.all = GetAOITimelineData(posterVBorders = AOIName,
#'locations = locations_testing)


### Get scanpath
GetAOITimelineData <- function(posterVBorders = posterVBorders, locations = locations, threshold = 3, frequency = 30, adjust = TRUE){

  if(adjust){
  posterVBorders$y = 480 - posterVBorders$y
  }
  # Data cleaning: add column names and remove NaNs and 0s
  if (ncol(locations) == 3) {
    colnames(locations) = c("x", "y", "PupilSize")} else {
      colnames(locations) = c("x", "y")
    }

  locations = locations[(locations[, 1] != "NaN" & locations[, 2] != "NaN"), ]
  locations = locations[(locations[, 1] != 0 | locations[, 2] != 0), ]

  # Get the region borders in terms of x and y
  regions = stats::aggregate(cbind(as.numeric(x), as.numeric(y)) ~ section,
                      data = posterVBorders, FUN = range)
  colnames(regions) = c("section", "x", "y")
  colnames(regions$x) = c("xmin", "xmax")
  colnames(regions$y) = c("ymin", "ymax")

  # Set the empty dataframe to output
  sec.num = nrow(regions)
  region = rep(0, nrow(locations))

  for(j in 1:nrow(locations)) {

    for(i in 1:sec.num){

      if (locations$x[j] >= regions$x[i, 1] & locations$x[j] <= regions$x[i, 2]
          & locations$y[j] >= regions$y[i, 1] & locations$y[j] <= regions$y[i, 2]) {

        region[j] =  as.character(regions$section[i])

        break

      }
    }

  }

  same_region = rle(region)
  dat.names = same_region$values
  locx = sapply(dat.names,function(x) NULL)
  locy = sapply(dat.names,function(x) NULL)

  if (ncol(locations) == 3) {

    # Create an empty list to store pupil data
    pupildat = sapply(dat.names,function(x) NULL)

    # Summarize the pupil size
    lengthvec = c(0, cumsum(same_region$length))

    for (i in 1:(length(lengthvec) - 1)){

      pupildat[[i]] = locations[(lengthvec[i] + 1):(lengthvec[i + 1]), 3]
      pupildat[[i]] = pupildat[[i]][pupildat[[i]] != -2000.00]
      locx[[i]] = locations[(lengthvec[i] + 1):(lengthvec[i + 1]), "x"]
      locy[[i]] = locations[(lengthvec[i] + 1):(lengthvec[i + 1]), "y"]

    }

    pupildat.blank = as.vector(unlist(pupildat[same_region$values == "0"]))
    pupildat = pupildat[same_region$length >= threshold]
    locx.blank = locx[same_region$values == "0"]
    locy.blank = locy[same_region$values == "0"]
    locx = locx[same_region$length >= threshold]
    locy = locy[same_region$length >= threshold]
    same_region$values = same_region$values[same_region$length >= threshold]
    same_region$length = same_region$length[same_region$length >= threshold]
    blank.num = sum(same_region$values == "0")
    blank.time = sum(same_region$lengths[same_region$values == "0"])/frequency
    blank = c(blank.time, blank.num)
    pupildat = pupildat[same_region$values != "0"]
    locx = locx[same_region$values != "0"]
    locy = locy[same_region$values != "0"]
    # Remove the blank AOI
    same_region$length = same_region$length[same_region$values != "0"]
    same_region$values = same_region$values[same_region$values != "0"]

  } else {

    lengthvec = c(0, cumsum(same_region$length))

    for (i in 1:(length(lengthvec) - 1)){

      locx[[i]] = locations[(lengthvec[i] + 1):(lengthvec[i + 1]), "x"]
      locy[[i]] = locations[(lengthvec[i] + 1):(lengthvec[i + 1]), "y"]

    }

    locx.blank = locx[same_region$values == "0"]
    locy.blank = locy[same_region$values == "0"]
    locx = locx[same_region$length >= threshold]
    locy = locy[same_region$length >= threshold]
    same_region$values = same_region$values[same_region$length >= threshold]
    same_region$length = same_region$length[same_region$length >= threshold]

    locx = locx[same_region$values != "0"]
    locy = locy[same_region$values != "0"]
    # Remove the blank AOI
    same_region$length = same_region$length[same_region$values != "0"]
    same_region$values = same_region$values[same_region$values != "0"]
    pupildat.blank = NA
  }

  # Get scanpath data
  scanpathdat = data.frame(matrix(rep(0, length(same_region$values)*2), nrow = length(same_region$values), ncol = 2))
  colnames(scanpathdat) = c("section", "visits")
  scanpathdat$section[1:length(same_region$values)] = as.character(same_region$values)
  scanpathdat$visits[1:length(same_region$values)] = same_region$length

  i = 1

  while (i < nrow(scanpathdat)) {

    while (scanpathdat$section[i] == scanpathdat$section[i + 1] & i < nrow(scanpathdat)){

      scanpathdat$visits[i] =  scanpathdat$visits[i] + scanpathdat$visits[i + 1]
      templist = list(c(unlist(pupildat[i], use.names = F), unlist(pupildat[i + 1], use.names = F)))
      templistx = list(c(unlist(locx[i], use.names = F), unlist(locx[i + 1], use.names = F)))
      templisty = list(c(unlist(locy[i], use.names = F), unlist(locy[i + 1], use.names = F)))

      names(templist) = names(pupildat[i])
      names(templistx) = names(locx[i])
      names(templisty) = names(locy[i])
      pupildat[i] = templist
      locx[i] = templistx
      locy[i] = templisty

      pupildat = pupildat[-(i + 1)]
      locx = locx[-(i + 1)]
      locy = locy[-(i + 1)]
      scanpathdat = scanpathdat[-(i+1), ]

    }

    i = i + 1

  }

  scanpathdat$sequence = 1:nrow(scanpathdat)
  scanpathdat$visits = scanpathdat$visits/frequency

  # Calculate eye movement speed in each AOI
  speed = sapply(scanpathdat$section, function(x) NULL)

  for (i in 1:nrow(scanpathdat)){

    speed[[i]] = sqrt((diff(locx[[i]]))^2 + (diff(locy[[i]]))^2)

  }

  speed.blank = vector("list", length(locx.blank))
  for (i in 1:length(locx.blank)){
  speed.blank[[i]] = sqrt((diff(locx.blank[[i]]))^2 + (diff(locy.blank[[i]]))^2)
  }
  # Create a list for blank data
  speed.blank = unlist(speed.blank)
  blank = list(blank, speed.blank, pupildat.blank)

  if (ncol(locations) == 3){
  return(list(scanpathdat, speed, blank, pupildat))
  } else {
  return(list(scanpathdat, speed, blank))
  }
}


#' GetPosterData
#'
#' Summarize poster viewing data according to the AOIs drawn
#'
#' The fist element of the list is a data frame contains for how long and how many times participant looked at each AOIs ;
#' The second element of the list is a list of eye movement speed in pixels for the visit at each AOIs;
#' The third element of the list is a list of pupil radiuses for each visit at the coorespoinding AOI, if the pupil radius is provided in the location csv file.
#'
#'
#'@param posterVBorders The output csv file from the \code{\link{DrawAOIs}} function.
#'@param locations The location and name of the csv file of the eye tracking data coordinates.
#'@param threshold Defines for at least how many images the coordinates fall into a certain AOI
#' to be considered as a visit. Default is 3.
#'@param frequency The frequency of the eye tracker. Default is 30.
#'@param return.blank A logical variable determines whether to return the statistics of
#'when the participant look at other areas than the defined AOIs. Default is TRUE.
#'@param adjust Adjust the difference in the coordinates from the results returned by Matlab. Default is TRUE.
#'@return A list of two or three data frames and lists with summarized statistics of viewing data according to the AOIs drawn.
#'@author Chunyang Li < lichunyang1990@hotmail.com >
#'@export GetPosterData
#'@examples
#'posterdat.all = GetPosterData(posterVBorders = AOIName,
#'                             locations = locations_testing)




### Get posterdat

GetPosterData <- function(posterVBorders = posterVBorders,
                               locations = locations,
                               threshold = 3,
                               frequency = 30,
                               return.blank = TRUE,
                               adjust = TRUE){

  datsummary = GetAOITimelineData(posterVBorders, locations, threshold = threshold, frequency = frequency, adjust = adjust)
  scanpathdat = datsummary[[1]]
  speed = datsummary[[2]]
  blank = datsummary[[3]]
  blank.row = c("Blank", blank[[1]])

  if (ncol(locations) == 3){
  pupildat = datsummary[[4]]
  }

  # Data sorted with alphabetic order by section names
  posterdat = stats::aggregate(visits~section, data = scanpathdat, FUN = sum)
  posterdat$visits.num = summary(as.factor(scanpathdat$section))

  if(return.blank){
  posterdat = rbind(posterdat, blank.row)
  }
  posterdat$visits = as.numeric(posterdat$visits)
  posterdat$visits.num = as.numeric(posterdat$visits.num)

  # Reorganize the list
  keys = unique(names(speed))
  speed = sapply(keys, function(name) {unlist(speed[grep(name, names(speed))], use.names = FALSE)})
  speed = speed[order(names(speed))]

  if(return.blank){
    speed$Blank = blank[[2]]
  }

  if (ncol(locations) == 3){
  pupildat = sapply(keys, function(name) {unlist(pupildat[grep(name, names(pupildat))], use.names = FALSE)})
  pupildat = pupildat[order(names(pupildat))]
     if(return.blank & length(blank) == 3){
       pupildat$Blank = blank[[3]]
     }
  }



  if (ncol(locations) == 3){
    return(list(posterdat, speed, pupildat))
  } else {
    return(list(posterdat, speed))
  }

}



