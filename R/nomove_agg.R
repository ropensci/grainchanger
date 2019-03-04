#' Upscale the function value
#' 
#' Calculate the value for a given function for each cell in a larger resolution grid. 
#'@param grid the grid across which to calculate the upscaled moving window function (Raster or SpatialPolygon)
#'@param dat The raster dataset on which to calculate the moving window function
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return Raster (if input is Raster) or numeric vector (if input is sp or sf object) containing values calculated for each coarser cell
#'@export
nomove_agg <- function(g, dat, fun, ...) {
  checkmate::assert(
    checkmate::checkClass(g, "RasterLayer"), 
    checkmate::checkClass(g, "SpatialPolygonsDataFrame"),
    checkmate::checkClass(g, "sf")
  )
  
  checkmate::assertClass(dat, "RasterLayer")
  
  if(fun == "shei") fun <- "nm_shei"
  
  # raster aggregation just acts as a wrapper for raster::aggregate (but it's less difficult to work out)
  if("RasterLayer" %in% class(g)) {
    a <- raster::res(g) / raster::res(dat)
    fun_args <- list(...)
    out <- raster::aggregate(dat, fact = a, fun=function(x, ...){do.call(get(fun), c(list(x), fun_args))})
  } 
  
  # sp object gets converted to sf first
  if("SpatialPolygonsDataFrame" %in% class(g)) g <- sf::st_as_sf(g)
  
  # aggregation to a polygon does some cropping and calculating
  if("sf" %in% class(g)) {
    out <- furrr::future_map_dbl(sf::st_geometry(g), function(grid_cell, dat, fun, ...) {
      grid_cell <- grid_cell %>% sf::st_geometry() %>% sf::st_sf()
      dat_cell <- raster::crop(dat, grid_cell)
      value <- get(fun)(as.vector(dat_cell), ...)
    }, dat, fun, ...)
  }
  
  return(out)
}