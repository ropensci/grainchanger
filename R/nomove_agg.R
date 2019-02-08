#' Upscale the function value
#' 
#' Calculate the value for a given function for each cell in a larger resolution grid. 
#'@param grid the grid across which to calculate the upscaled moving window function (Raster or SpatialPolygon)
#'@param dat The raster dataset on which to calculate the moving window function
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return Raster (if input is Raster) or numeric vector (if input is SpatialPolygon) containing values calculated for each coarser cell
#'@export
nomove_agg <- function(g, dat, fun, ...) {
  # raster aggregation just acts as a wrapper for raster::aggregate (but it's less difficult to work out)
  if("RasterLayer" %in% class(g)) {
    a <- res(g) / res(dat)
    fun_args <- list(...)
    out <- raster::aggregate(dat, fact = a, fun=function(x, ...){do.call(get(fun), c(list(x), fun_args))})
  } 
  
  # aggregation to a polygon does some cropping and calculating
  if("sf" %in% class(g)) {
    out <- furrr::future_map_dbl(st_geometry(g), function(grid_cell, dat, fun, ...) {
      grid_cell <- grid_cell %>% sf::st_geometry() %>% sf::st_sf()
      dat_cell <- raster::crop(dat, grid_cell)
      value <- get(fun)(as.vector(dat_cell), ...)
    }, dat, fun, ...)
  }
  return(out)
}