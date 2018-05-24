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

winmove_cell <- function(cell, grid, dat, radius, type, fn, ...) {
  grid_cell <- grid[cell, ]
  # i'm currently adding a buffer to remove edge effects and actually
  # incorporate the outside effect, this will then be removed. Need to think
  # about how this can be changed to allow for other uses
  # i can do this using pad = T but at the moment not sure I want that functionality available.
  grid_buffer <- rgeos::gBuffer(grid_cell, width = radius, capStyle = "SQUARE", joinStyle = "MITRE", mitreLimit = radius/2)
  dat_cell <- raster::crop(dat, grid_buffer) 
  
  wdw <- raster::focalWeight(dat_cell, radius, type=type)
  wdw <- ifelse(wdw > 0, 1, NA)
  out <- raster::focal(dat, wdw, function(x, ...) {get(fn)(x, ...)}, na.rm = TRUE, ...)
  return(out)
}