#' Direct data aggregation
#'
#' Calculate the value for a given function for each cell in a larger resolution grid.
#'
#' @param coarse_dat  sf, Raster* or Spatial* object. The coarse grain data (response
#'   data) across which to calculate the aggregated function
#' @param fine_dat Raster* object. Raster* object. The fine grain data (predictor /
#'   covariate data) to aggregate
#' @param fun character. The function to apply. The function fun should take multiple numbers, and
#'   return a single number. For example mean, modal, min or max. It should also accept a
#'   na.rm argument (or ignore it, e.g. as one of the 'dots' arguments. For example,
#'   length will fail, but function(x, ...){na.omit(length(x))} works. See Details
#'  @param is_grid logical. Use \code{TRUE} (default) if \code{g} contains only rectangular
#'   cells (i.e. a grid). If \code{g} is any other polygon file, this should be set to
#'   false
#' @param quiet logical. If \code{FALSE} (default) and \code{is_grid == TRUE} the user
#'   gets a warning that the aggregation assumes all cells are rectangular
#' @param ... further arguments passed to or from other methods
#'
#' @return Raster (if input is Raster) or numeric vector (if input is sp or sf object)
#'   containing values calculated for each coarser cell
#'
#' @keywords spatial, aggregate
#'
#' @details \code{grainchanger} has several built-in functions. Functions currently
#'   included are: \itemize{ 
#'      \item \code{shei} - Shannon evenness, requires the additional argument \code{lc_class} (vector or scalar) 
#'      \item \code{prop} - Proportion, requires the additional argument \code{lc_class} (scalar) 
#'      \item \code{classes} - Unique number of classes in a categorical landscape 
#'      \item \code{var_range} - Range (max - min) 
#'   }
#'
#' @examples
#' # load required data
#' data(g_sf)
#' data(cont_ls)
#' data(cat_ls)
#'
#' # aggregate using mean
#' d <- nomove_agg(g_sf, cont_ls, "mean")
#'
#' # aggregate using Shannon evenness
#' d <- nomove_agg(g_sf, cont_ls, "nm_shei", lc_class = 0:3)
#' @export

nomove_agg <- function(coarse_dat, 
                       fine_dat, 
                       fun, 
                       is_grid = TRUE, 
                       quiet = FALSE, 
                       g, dat, ...) {
  
  # code to deal with old parameter names
  if (!missing(g)) {
    warning("use 'coarse_dat' instead of 'g'\n")
    coarse_dat <- g
  }
  
  if (!missing(dat)) {
    warning("use 'fine_dat' instead of 'dat'\n")
    fine_dat <- dat
  }

  checkmate::assert(
    checkmate::check_class(coarse_dat, "RasterLayer"),
    checkmate::check_class(coarse_dat, "SpatialPolygonsDataFrame"),
    checkmate::check_class(coarse_dat, "sf"),
  )

  checkmate::assert_class(fine_dat, "RasterLayer")
  
  if(is_grid & !quiet) warning("WARNING: aggregation assumes all cells are rectangular set is_grid = FALSE if g is not a grid")

  if (fun == "shei") fun <- "nm_shei"
  if (fun == "prop") fun <- "nm_prop"
  
  # raster aggregation just acts as a wrapper for raster::aggregate (but it's less difficult to work out)
  if ("RasterLayer" %in% class(coarse_dat)) {
    a <- raster::res(coarse_dat) / raster::res(fine_dat)
    fun_args <- list(...)
    out <- raster::aggregate(fine_dat, fact = a, fun = function(x, ...) {
      do.call(get(fun), c(list(x), fun_args))
    })
  }

  # sp object gets converted to sf first
  if ("SpatialPolygonsDataFrame" %in% class(coarse_dat)) coarse_dat <- sf::st_as_sf(coarse_dat)

  # aggregation to a polygon does some cropping and calculating
  if ("sf" %in% class(coarse_dat)) {
    out <- furrr::future_map_dbl(sf::st_geometry(coarse_dat), function(grid_cell, dat, fun, ...) {
      grid_cell <- sf::st_geometry(grid_cell)
      grid_cell_sf <- sf::st_sf(grid_cell)
      dat_cell <- raster::crop(dat, grid_cell_sf)  
      if(!is_grid) {
        # some concerns here that when the input data contains NA, it will be removed from the
        # calculation of total area. Needs thought.
        dat_cell <- raster::mask(dat_cell, grid_cell_sf)   # mask is slower, but needs to be used if polygons are not rectangular
        dat_cell <- raster::values(dat_cell)
        dat_cell <- na.omit(dat_cell)
      }
      value <- get(fun)(dat_cell, ...)
    }, fine_dat, fun, ...)
  }

  return(out)
}
