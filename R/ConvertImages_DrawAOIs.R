#########################
# Resize image
#########################

#' ResizeImg
#'
#' Resize the image into the specified dimensions
#'
#'@param poster the location and name of the poster image file.
#'@param size_x the width of the new image. Default is 640.
#'@param size_y the height of the new image. Default is 480.
#'@param resized_poster the directory and the name of the output jpg file.
#'@return A resized the image in the specified dimensions
#'@export ResizeImg
#'@author Chunyang Li < lichunyang1990@hotmail.com >
#'@examples
#'ResizeImg(poster = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"),
#'size_x = 500, size_y = 400, resized_poster = "poster_colored2.jpg")

ResizeImg <- function(poster, size_x = 640, size_y = 480, resized_poster){

  poster = imager::load.image(poster)
  poster = imager::resize(poster, size_x, size_y)
  imager::save.image(poster, resized_poster)

}

#########################
# DrawAOIs
#########################


#' DrawAOIs
#'
#' Definie rectangular Area of Interests by clicking the vertex of the rectangle and type the names in the R console
#'
#'@param boxes Define how many area of interests one is going to create.
#'@param poster The location and name of the poster image file.
#'@param out The name of the output csv file. Default is "RegionName.csv"
#'@return RegionBorder The border information of the defined area of interests.
#'@author Chunyang Li < lichunyang1990@hotmail.com >
#'@export DrawAOIs
#'@examples
#'\dontrun{
#'AOIdata <- DrawAOIs(boxes = 2,
#' poster = system.file("extdata", "poster_colored.jpg", package = "EyeTrackR"))
#'}

DrawAOIs <- function(boxes, poster, out = "RegionName.csv"){

  poster <- imager::load.image(poster)
  graphics::par(mar = c(0, 0, 0, 0) + 0.1)
  graphics::plot(poster, xlab = "", ylab = "", axes = FALSE)
  graphics::rect(0, 0, imager::width(poster), imager::height(poster))
  ##
  corners <- function() {
    loc1 <- graphics::locator(1)
    loc2 <- graphics::locator(1)
    coos <- c(unlist(loc1), unlist(loc2))
    graphics::rect(coos[1], coos[2], coos[3], coos[4], border = 'red')
    return(coos)
  }
  ##

  readname <- function()
  {
    n <- readline(prompt = "Enter the region name: ")
    return(n)
  }


  RegionBorder = as.data.frame(matrix(rep(0, 3*boxes*6), nrow = boxes*6, ncol = 3))
  colnames(RegionBorder) = c("section", "x", "y")


  for (i in 1:boxes){

    print("Please select a rectangular area of interest by mouse clicking two diagonal vertices")
    aoi = corners()
    aoi.name = readname()

    while (nchar(aoi.name) == 0){
      print("You have to input a non-empty name for the region!")
      aoi.name = readname()
    }

    RegionBorder[((i - 1)*6 + 1):(i*6), 1] = rep(aoi.name, 6)
    RegionBorder[(i - 1)*6 + 1, 2:3] = c(min(aoi[1], aoi[3]), min(aoi[2], aoi[4]))
    RegionBorder[(i - 1)*6 + 2, 2:3] = c(max(aoi[1], aoi[3]), min(aoi[2], aoi[4]))
    RegionBorder[(i - 1)*6 + 3, 2:3] = c(max(aoi[1], aoi[3]), max(aoi[2], aoi[4]))
    RegionBorder[(i - 1)*6 + 4, 2:3] = c(min(aoi[1], aoi[3]), max(aoi[2], aoi[4]))
    RegionBorder[(i - 1)*6 + 5, 2:3] = c(min(aoi[1], aoi[3]), min(aoi[2], aoi[4]))
    RegionBorder[(i - 1)*6 + 6, 2:3] = c("NA", "NA")

  }

  utils::write.table(RegionBorder, file = out, sep = ",", row.names = F)
  return(RegionBorder)

}




