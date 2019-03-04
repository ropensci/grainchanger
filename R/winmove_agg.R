#'Moving-window aggregation
#'
#'Calculate the mean moving window value for a given radius/shape/function for each cell
#'in a larger resolution grid.
#'@param g the grid across which to calculate the upscaled moving window function
#'  (raster, SpatialPolygonsDataFrame, or sf object)
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
  checkmate::assert(
    checkmate::checkClass(g, "RasterLayer"), 
    checkmate::checkClass(g, "SpatialPolygonsDataFrame"),
    checkmate::checkClass(g, "sf")
  )
  
  checkmate::assertClass(dat, "RasterLayer")
  checkmate::assertCount(d)
  
  # convert raster to grid
  if("RasterLayer" %in% class(g)) {
    g <- as(g, "SpatialPolygonsDataFrame")
  }
  
  if("SpatialPolygonsDataFrame" %in% class(g)) {
    g <- sf::st_as_sf(g)
  }
  
  out <- furrr::future_map_dbl(sf::st_geometry(g), function(grid_cell, dat, d, type, fun, ...) {
    grid_buffer <- sf::st_buffer(grid_cell, dist = d, endCapStyle = "SQUARE", joinStyle = "MITRE", mitreLimit = d/2)
    grid_buffer <- sf::st_geometry(grid_buffer)
    grid_buffer <- sf::st_sf(grid_buffer)
    dat_cell <- raster::crop(dat, grid_buffer)
    winmove_cellr <- winmove(dat_cell, d, type, fun, ...)
    mean(raster::values(winmove_cellr), na.rm=TRUE)
  }, dat, d, type, fun, ...)
  
  return(out)
}