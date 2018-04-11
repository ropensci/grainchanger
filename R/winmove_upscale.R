#' Upscale the moving window value
#' 
#' Calculate the mean moving window value for a given radius/shape/function for each cell in a larger resolution grid. 
#'@param grid the grid across which to calculate the upscaled moving window function
#'@param dat The raster dataset on which to calculate the moving window function
#'@param radius The radius of the moving window
#'@param type The shape of the moving window
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return Numeric vector containing moving window values calculated for each grid cell
#'@export

winmove_upscale <- function(grid, dat, radius, type, fn, ...) {
  cell_div <- pbapply::pblapply(1:nrow(grid), winmove_cell, grid, dat, radius, type, fn, ...)
  cell_mean <- sapply(cell_div, winmove_cellmean)
  return(as.numeric(cell_mean))
}