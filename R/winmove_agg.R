#' Moving-window data aggregation
#'
#' Calculate the mean moving window value for a given radius, shape and function for each cell
#' in a larger resolution grid.
#'
#' @param g the grid across which to calculate the aggregated moving window function
#'  (raster, SpatialPolygonsDataFrame, or sf object)
#' @param dat The raster dataset to aggregate
#' @param d numeric. If \code{type=circle}, the radius of the circle (in units of the CRS). If
#'  \code{type=rectangle} the dimension of the rectangle (one or two numbers). If \code{type=Gauss}
#'  the size of sigma, and optionally another number to determine the size of the matrix
#'  returned (default is 3 times sigma)
#' @param type The shape of the moving window
#' @param fun The function to apply. The function fun should take multiple numbers, and
#'  return a single number. For example mean, modal, min or max. It should also accept a
#'  na.rm argument (or ignore it, e.g. as one of the 'dots' arguments. For example, length
#'  will fail, but function(x, ...){na.omit(length(x))} works. See Details
#' @param is_grid Use \code{TRUE} if \code{g} contains only rectangular cells (i.e. a
#'   grid). If \code{g} is any other polygon file, this should be set to false
#' @param ... further arguments passed to or from other methods
#'
#' @return Numeric vector containing moving window values calculated for each grid cell
#'
#' @keywords focal, spatial, aggregate
#'
#' @details \code{grainchanger} has several built-in functions. Functions currently included are:
#' \itemize{
#' \item \code{wm_shei} - Shannon evenness, requires the additional argument \code{lc_class} (vector or scalar)
#' \item \code{wm_prop} - Proportion, requires the additional argument \code{lc_class} (scalar)
#' \item \code{wm_classes} - Unique number of classes in a categorical landscape
#' \item \code{var_range} - Range (max - min)
#' }
#' @examples
#' # load required data
#' data(g_sf)
#' data(cont_ls)
#' data(cat_ls)
#' 
#' #' # aggregate using mean
#' d <- winmove_agg(g_sf, cont_ls, 5, "rectangle", "mean")
#' 
#' # aggregate using Shannon evenness
#' d <- winmove_agg(g_sf, cat_ls, 5, "rectangle", "shei", lc_class = 0:3)
#' @export

winmove_agg <- function(g, dat, d, type, fun, is_grid = TRUE, ...) {
  checkmate::assert(
    checkmate::check_class(g, "RasterLayer"),
    checkmate::check_class(g, "SpatialPolygonsDataFrame"),
    checkmate::check_class(g, "sf")
  )

  checkmate::assert_class(dat, "RasterLayer")
  checkmate::assert_numeric(d)

  if(is_grid) warning("aggregation assumes all cells are rectangular\nset is_grid = FALSE if g is not a grid")
  
  # convert raster to grid
  if ("RasterLayer" %in% class(g)) {
    g <- methods::as(g, "SpatialPolygonsDataFrame")
  }

  if ("SpatialPolygonsDataFrame" %in% class(g)) {
    g <- sf::st_as_sf(g)
  }

  out <- furrr::future_map_dbl(sf::st_geometry(g), function(grid_cell, dat, d, type, fun, agg_fun, ...) {
    grid_buffer <- sf::st_buffer(grid_cell, dist = d, endCapStyle = "SQUARE", joinStyle = "MITRE", mitreLimit = d / 2)
    grid_buffer <- sf::st_geometry(grid_buffer)
    grid_buffer <- sf::st_sf(grid_buffer)
    dat_cell <- raster::crop(dat, grid_buffer)  
    if(!is_grid) {
      dat_cell <- raster::mask(dat_cell, grid_buffer)   # mask is slower, but needs to be used if polygons are not rectangular
    }
    winmove_cellr <- winmove(dat_cell, d, type, fun, ...)
    get(agg_fun)(raster::values(winmove_cellr), na.rm = TRUE)
  }, dat, d, type, fun, ...)

  return(out)
}
