#' Calculate the mean value of an input raster
#' 
#' This function takes a raster as input and calculates it's mean value. For my purposes, this is used to upscale.
#' @param winmove_cellr raster. 
#' @return numeric. The mean value of the input raster
#' @export

winmove_cellmean <- function(winmove_cellr) {
  out <- mean(raster::as.matrix(winmove_cellr), na.rm=TRUE)
  return(out)
}