#'Calculate the value of a given function for a raster within a particular cell of a larger grid
#'
#'This function takes as input the row identifier of a cell within a large resolution grid and calculates the value for a given function for a smaller resolution raster dataset. 
#'@param cell the row identifier of the grid cell
#'@param grid the grid from which to take the cell
#'@param dat The raster dataset on which to calculate the function
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return numeric. The value of the function applied to the raster within the larger resolution cell
#'@export
nomove_cell <- function(cell, grid, dat, fn, ...) {
  grid_cell <- grid[cell, ]
  dat_cell <- raster::crop(dat, grid_cell)
  if(fn == "diversity") {
    out <- diversity(dat_cell, ...)
  } else if (fn == "prop") {
    out <- sum(as.vector(dat_cell == lc_class)) / raster::ncell(dat_cell) 
  } else {
    out <- get(fn)(as.vector(dat_cell), ...)
  }
  return(out)
}
