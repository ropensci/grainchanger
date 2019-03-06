#'Direct data aggregation
#'
#'Calculate the value for a given function for each cell in a larger resolution grid.
#'
#'@param g the grid across which to calculate the aggregated moving window function
#'  (raster, SpatialPolygonsDataFrame, or sf object)
#'@param dat The raster dataset to aggregate
#'@param fun The function to apply. The function fun should take multiple numbers, and
#'  return a single number. For example mean, modal, min or max. It should also accept a
#'  na.rm argument (or ignore it, e.g. as one of the 'dots' arguments. For example, length
#'  will fail, but function(x, ...){na.omit(length(x))} works. See Details
#'@param ... further arguments passed to or from other methods
#'
#'@return Raster (if input is Raster) or numeric vector (if input is sp or sf object)
#'  containing values calculated for each coarser cell
#' 
#'@keywords spatial, aggregate
#'   
#'@details \code{grainchanger} has several built-in functions. Functions currently included are: 
#'\itemize{
#' \item \code{nm_shei} - Shannon evenness, requires the additional argument \code{lc_class} (vector or scalar)
#' \item \code{nm_prop} - Proportion, requires the additional argument \code{lc_class} (scalar)
#' \item \code{var_range} - Range (max - min) 
#'}
#' 
#'@examples
#'# load required data
#'data(g_sf)
#'data(cont_ls)
#'data(cat_ls)
#'
#'# aggregate using mean
#'d = nomove_agg(g_sf, cont_ls, "mean")
#'
#'# aggregate using Shannon evenness
#'d = nomove_agg(g_sf, cont_ls, "nm_shei", lc_class = 0:3)
#'
#'@export

nomove_agg <- function(g, dat, fun, ...) {
  checkmate::assert(
    checkmate::check_class(g, "RasterLayer"), 
    checkmate::check_class(g, "SpatialPolygonsDataFrame"),
    checkmate::check_class(g, "sf")
  )
  
  checkmate::assert_class(dat, "RasterLayer")
  
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
      grid_cell <-  sf::st_geometry(grid_cell)
      grid_cell_sf <- sf::st_sf(grid_cell)
      dat_cell <- raster::crop(dat, grid_cell_sf)
      value <- get(fun)(as.vector(dat_cell), ...)
    }, dat, fun, ...)
  }
  
  return(out)
}