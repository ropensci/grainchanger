#' Upscale the function value
#' 
#' Calculate the value for a given function for each cell in a larger resolution grid. 
#'@param grid the grid across which to calculate the upscaled moving window function
#'@param dat The raster dataset on which to calculate the moving window function
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return Numeric vector containing values calculated for each grid cell
#'@export
nomove_upscale <- function(grid, dat, fn, ...) {
  cell_values <- pbapply::pbsapply(1:nrow(grid), nomove_cell, grid, dat, fn, ...)
  return(cell_values)
}
