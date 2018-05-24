#' Upscale the function value
#' 
#' Calculate the value for a given function for each cell in a larger resolution grid. 
#'@param grid the grid across which to calculate the upscaled moving window function (Raster or SpatialPolygon)
#'@param dat The raster dataset on which to calculate the moving window function
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return Raster (if input is Raster) or numeric vector (if input is SpatialPolygon) containing values calculated for each coarser cell
#'@export
nomove_agg <- function(grid, dat, fn, ...) {
  grid_types <- c("RasterLayer", "SpatialPolygonsDataFrame")
  match.arg(class(grid), grid_types)
  
  # raster aggregation just acts as a wrapper for raster::aggregate (but it's less difficult to work out)
  if(class(grid) == "RasterLayer") {
    a <- res(grid) / res(dat)
    fun_args <- list(...)
    cell_values <- raster::aggregate(dat, fact = a, fun=function(x, ...){do.call(get(fn), c(list(x), fun_args))})
  } 
  
  # aggregation to a polygon does some cropping and calculating
  if(class(grid) == "SpatialPolygonsDataFrame") {
    cell_values <- sapply(1:nrow(grid), function(cell) {
      grid_cell <- grid[cell, ]
      dat_cell <- raster::crop(dat, grid_cell)
      out <- get(fn)(as.vector(dat_cell), ...)
    })
  }
  return(cell_values)
}

out <- raster::focal(dat, wdw, function(x) {get(fn)(x, ...)})