#'Calculate the value of a given function for each cell, within a given neighbourhood. 
#'
#'This function takes as input a raster dataset, the radius and type of moving window (`circle`, `rectangle` or `Gauss`) and a function to apply across the moving window. A new raster with the moving window value for each cell is then output.
#'@param dat The raster dataset on which to calculate the moving window function
#'@param radius The radius of the moving window
#'@param type The shape of the moving window
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return A raster with the moving window values calculated
#'@export

winmove_nbrhd <- function(dat, radius, type=c("circle", "Gauss", "rectangle"), fn, ...) {
  nbrhd <- raster::focalWeight(dat, radius, type=type)
  
  if(fn == "diversity") {
    out <- diversity_nbrhd(dat, nbrhd, ...)
  } else {
    out <- raster::focal(dat, nbrhd, get(fn), ...)
  }
  return(out)
}