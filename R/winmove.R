#'Calculate the value of a given function for each cell, within a given neighbourhood, for a cell in a larger resolution grid. 
#'
#'This function takes as input the row identifier of a cell within a large resolution grid and calculates the moving window function for a smaller resolution raster dataset using the `winmove_nbrhd()` function.
#'@param cell the row identifier of the grid cell
#'@param grid the grid from which to take the cell
#'@param dat The raster dataset on which to calculate the moving window function
#'@param d numeric. If `type=circle`, the radius of the circle (in units of the CRS). If
#'  `type=rectangle` the dimension of the rectangle (one or two numbers). If `type=Gauss` the
#'  size of sigma, and optionally another number to determine the size of the matrix
#'  returned (default is 3 times sigma)
#'@param type The shape of the moving window
#'@param fun The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return A raster with the moving window values calculated
#'@export

winmove <- function(dat, d, type, fun, ...) {

  checkmate::assertClass(dat, "RasterLayer")
  checkmate::assertCount(d)
  
  if(fun == "prop") {
    out <- wm_prop(dat, d, type, ...)
    return(out)
  } 
  
  if(fun == "mean") {
    out <- wm_mean(dat, d, type)
    return(out)
  }
  
  if(fun == "shei") {
    out <- wm_shei(dat, d, type, ...)
    return(out)
  }

  # this catches all others (i.e. in-built or user-defined functions)
  w <- raster::focalWeight(dat, d, type=type)
  w <- ifelse(w > 0, 1, NA)
  out <- raster::focal(dat, w, function(x) {get(fun)(x, na.rm = TRUE, ...)})
  return(out)
}
