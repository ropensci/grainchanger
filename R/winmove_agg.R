#' Moving-window aggregation
#' 
#' Calculate the mean moving window value for a given radius/shape/function for each cell in a larger resolution grid. 
#'@param grid the grid across which to calculate the upscaled moving window function (raster or SpatialPolygon)
#'@param dat The raster dataset on which to calculate the moving window function
#'@param radius The radius of the moving window
#'@param type The shape of the moving window
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return Numeric vector containing moving window values calculated for each grid cell
#'@export

winmove_agg <- function(grid, dat, radius, type, fn, ...) {
  # convert raster to grid
  if(class(grid) == "RasterLayer") {
    grid <- as(grid, "SpatialPolygonsDataFrame")
  }
  
  out <- sapply(1:nrow(grid), function(cell) {
    grid_cell <- grid[cell, ]
    grid_buffer <- rgeos::gBuffer(grid_cell, width = radius, capStyle = "SQUARE", joinStyle = "MITRE", mitreLimit = radius/2)
    dat_cell <- raster::crop(dat, grid_buffer) 
    winmove_cellr <- winmove(dat_cell, radius, type, fn, ...)
    mean(raster::values(winmove_cellr), na.rm=TRUE)
  })
  
  return(as.numeric(out))
}