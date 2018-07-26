#'Calculate the value of a given function for each cell, within a given neighbourhood, for a cell in a larger resolution grid. 
#'
#'This function takes as input the row identifier of a cell within a large resolution grid and calculates the moving window function for a smaller resolution raster dataset using the `winmove_nbrhd()` function.
#'@param cell the row identifier of the grid cell
#'@param grid the grid from which to take the cell
#'@param dat The raster dataset on which to calculate the moving window function
#'@param radius The radius of the moving window
#'@param type The shape of the moving window
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return A raster with the moving window values calculated
#'@export

winmove <- function(dat, radius, type, fn, ...) {
  wdw <- raster::focalWeight(dat, radius, type=type)
  wdw <- ifelse(wdw > 0, 1, NA)
  out <- raster::focal(dat, wdw, function(x) {get(fn)(x, na.rm = TRUE, ...)})
  return(out)
}
