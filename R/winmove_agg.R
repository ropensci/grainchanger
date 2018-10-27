#'Moving-window aggregation
#'
#'Calculate the mean moving window value for a given radius/shape/function for each cell
#'in a larger resolution grid.
#'@param g the grid across which to calculate the upscaled moving window function
#'  (raster or SpatialPolygon)
#'@param dat The raster dataset on which to calculate the moving window function
#'@param d numeric. If `type=circle`, the radius of the circle (in units of the CRS). If
#'  `type=rectangle` the dimension of the rectangle (one or two numbers). If `type=Gauss` the
#'  size of sigma, and optionally another number to determine the size of the matrix
#'  returned (default is 3 times sigma)
#'@param type The shape of the moving window
#'@param fun The function to apply
#'@param ... further arguments passed to or from other methods
#'@return Numeric vector containing moving window values calculated for each grid cell
#'@export

winmove_agg <- function(g, dat, d, type, fun, ...) {
  # convert raster to grid
  if(class(g) == "RasterLayer") {
    g <- as(g, "SpatialPolygonsDataFrame")
  }
  
  out <- furrr::future_map_dbl(1:nrow(g), function(cell, g, dat, d, type, fun, ...) {
    grid_cell <- g[cell, ]
    grid_buffer <- rgeos::gBuffer(grid_cell, width = d, capStyle = "SQUARE", joinStyle = "MITRE", mitreLimit = d/2)
    dat_cell <- raster::crop(dat, grid_buffer) 
    winmove_cellr <- winmove(dat_cell, d, type, fun, ...)
    mean(raster::values(winmove_cellr), na.rm=TRUE)
  }, g, dat, d, type, fun, ...)
  
  return(out)
}