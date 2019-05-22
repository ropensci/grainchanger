#' Moving-window data aggregation
#'
#' Calculate the mean moving window value for a given radius, shape and function for each
#' cell in a larger resolution grid.
#'
#' @param coarse_dat sf, Raster* or Spatial* object. The coarse grain data (response data)
#'   across which to calculate the aggregated moving window function
#' @param fine_dat Raster* object. The fine grain data (predictor / covariate data) to
#'   aggregate
#' @param d numeric. If \code{type=circle}, the radius of the circle (in units of the
#'   CRS). If \code{type=rectangle} the dimension of the rectangle (one or two numbers).
#'   If \code{type=Gauss} the size of sigma, and optionally another number to determine
#'   the size of the matrix returned (default is 3 times sigma)
#' @param type character. The shape of the moving window
#' @param win_fun character. The function to apply to the moving window. The function fun
#'   should take multiple numbers, and return a single number. For example mean, modal,
#'   min or max. It should also accept a na.rm argument (or ignore it, e.g. as one of the
#'   'dots' arguments. For example, length will fail, but function(x,
#'   ...){na.omit(length(x))} works. See Details
#' @param agg_fun character. The function by which to aggregate. By default this is set to
#'   \code{mean}
#' @param is_grid logical. Use \code{TRUE} (default) if \code{g} contains only rectangular
#'   cells (i.e. a grid). If \code{g} is any other polygon file, this should be set to
#'   false
#' @param quiet logical. If \code{FALSE} (default) and \code{is_grid == TRUE} the user
#'   gets a warning that the aggregation assumes all cells are rectangular
#' @param ... further arguments passed to or from other methods
#'
#' @return Numeric vector containing moving window values calculated for each grid cell
#'
#' @keywords focal, spatial, aggregate
#'
#' @details \code{grainchanger} has several built-in functions. Functions currently
#'   included are: \itemize{ 
#'      \item \code{shei} - Shannon evenness, requires the additional argument \code{lc_class} (vector or scalar) 
#'      \item \code{prop} - Proportion, requires the additional argument \code{lc_class} (scalar) 
#'      \item \code{classes} - Unique number of classes in a categorical landscape 
#'      \item \code{var_range} - Range (max - min) 
#'   }
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

winmove_agg <- function(coarse_dat, 
                        fine_dat, 
                        d, 
                        type=c('circle', 'Gauss', 'rectangle'), 
                        win_fun, 
                        agg_fun = "mean", 
                        is_grid = TRUE, 
                        quiet = FALSE,
                        ...) {
  
  checkmate::assert(
    checkmate::check_class(coarse_dat, "RasterLayer"),
    checkmate::check_class(coarse_dat, "SpatialPolygonsDataFrame"),
    checkmate::check_class(coarse_dat, "sf")
  )

  checkmate::assert_class(fine_dat, "RasterLayer")
  checkmate::assert_numeric(d)

  if(is_grid & !quiet) {
    usethis::ui_line("aggregation assumes all cells are rectangular")
    usethis::ui_todo("set `is_grid = FALSE` if coarse_dat is not a grid")
  }
  
  # convert raster to grid
  if ("RasterLayer" %in% class(coarse_dat)) {
    coarse_dat <- methods::as(coarse_dat, "SpatialPolygonsDataFrame")
  }

  if ("SpatialPolygonsDataFrame" %in% class(coarse_dat)) {
    coarse_dat <- sf::st_as_sf(coarse_dat)
  }

  out <- furrr::future_map_dbl(sf::st_geometry(coarse_dat), function(grid_cell, fine_dat, d, type, win_fun, agg_fun, ...) {
    grid_buffer <- sf::st_sf(
      sf::st_geometry(
          sf::st_buffer(grid_cell, dist = d, endCapStyle = "SQUARE", joinStyle = "MITRE", mitreLimit = d / 2)
      )
    )
    #grid_buffer <- sf::st_geometry(grid_buffer)
    if(is_grid) {
      dat_cell <- raster::crop(fine_dat, grid_buffer)
      dat_cell <- raster::extend(dat_cell, grid_buffer)
      win_cell <- winmove(dat_cell, d, type, win_fun, ...)
      get(agg_fun)(raster::values(win_cell), na.rm = TRUE)
    } else {
      dat_cell <- raster::crop(fine_dat, grid_buffer)
      dat_cell <- raster::extend(dat_cell, grid_buffer)
      dat_cell <- raster::mask(dat_cell, grid_buffer)
      win_cell <- winmove(dat_cell, d, type, win_fun, ...)
      get(agg_fun)(raster::values(win_cell), na.rm = TRUE)
    }
  }, fine_dat, d, type, win_fun, agg_fun, ...)

  return(out)
}
