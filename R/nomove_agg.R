#' Direct data aggregation
#'
#' Calculate the value for a given function for each cell in a larger resolution
#' grid.
#'
#' @param coarse_dat  sf, Raster* or Spatial* object. The coarse grain data
#'   (response data) across which to calculate the aggregated function
#' @param fine_dat Raster* object. Raster* object. The fine grain data
#'   (predictor / covariate data) to aggregate
#' @param agg_fun function The function to apply. The function fun should take
#'   multiple numbers, and return a single number. For example mean, modal, min
#'   or max. It should also accept a na.rm argument (or ignore it, e.g. as one
#'   of the 'dots' arguments. For example, length will fail, but function(x,
#'   ...){na.omit(length(x))} works. See Details
#' @param is_grid logical. Use \code{TRUE} (default) if \code{g} contains only
#'   rectangular cells (i.e. a grid). If \code{g} is any other polygon file,
#'   this should be set to false
#' @param quiet logical. If \code{FALSE} (default) and \code{is_grid == TRUE}
#'   the user gets a warning that the aggregation assumes all cells are
#'   rectangular
#' @param ... further arguments passed to or from other methods
#'
#' @return Raster (if input is Raster) or numeric vector (if input is sp or sf
#'   object) containing values calculated for each coarser cell
#'
#' @keywords spatial, aggregate
#'
#'#' @details \code{grainchanger} has several built-in functions. Functions
#'   currently included are: 
#'   \itemize{ 
#'      \item \code{shdi} - Shannon diversity, requires the additional argument \code{lc_class} (vector or scalar) 
#'      \item \code{shei} - Shannon evenness, requires the additional argument \code{lc_class} (vector or scalar) 
#'      \item \code{prop} - Proportion, requires the additional argument \code{lc_class} (scalar)
#'      \item \code{var_range} - Range (max - min) 
#'      }
#'      
#'  Note that \code{nomove_agg} can be run in parallel using \code{plan(multiprocess)} from the \code{future} package. 
#'  
#' @examples
#' # load required data
#' data(g_sf)
#' data(cont_ls)
#' data(cat_ls)
#'
#' # aggregate using mean
#' d <- nomove_agg(g_sf, cont_ls, mean)
#'
#' # aggregate using Shannon evenness
#' d <- nomove_agg(g_sf, cont_ls, shei, lc_class = 1:4)
#' @export

nomove_agg <- function(coarse_dat, 
                       fine_dat, 
                       agg_fun, 
                       is_grid = TRUE, 
                       quiet = FALSE, 
                       ...) {
  
  checkmate::assert(
    checkmate::check_class(coarse_dat, "RasterLayer"),
    checkmate::check_class(coarse_dat, "SpatialPolygonsDataFrame"),
    checkmate::check_class(coarse_dat, "sf"),
    checkmate::check_class(coarse_dat, "sfc")
  )

  checkmate::assert_class(fine_dat, "RasterLayer")
  checkmate::assert_function(agg_fun)
  
  if(is_grid & !quiet) {
    usethis::ui_line("aggregation assumes all cells are rectangular")
    usethis::ui_todo("set `is_grid = FALSE` if coarse_dat is not a grid")
  }

  # convert raster to grid
  raster_output <- FALSE
  if ("RasterLayer" %in% class(coarse_dat)) {
    raster_output <- TRUE
    ras <- coarse_dat
    coarse_dat <- methods::as(coarse_dat, "SpatialPolygonsDataFrame")
  }

  # sp object gets converted to sf first
  if ("SpatialPolygonsDataFrame" %in% class(coarse_dat)) {
    coarse_dat <- sf::st_as_sf(coarse_dat)
  }
  
  # aggregation to a polygon does some cropping and calculating
  
  out <- furrr::future_map_dbl(sf::st_geometry(coarse_dat), 
                               function(grid_cell, fine_dat, agg_fun, ...) {
                                 # buffer and crop
                                 grid_cell <- sf::st_geometry(grid_cell)
                                 grid_cell_sf <- sf::st_sf(grid_cell)
                                 dat_cell <- raster::crop(fine_dat, 
                                                          grid_cell_sf)  
                                 
                                 # mask if not rectangular grid
                                 if(!is_grid) {
                                   dat_cell <- raster::mask(dat_cell, 
                                                            grid_cell_sf)   
                                 }
                                 
                                 # calculate value
                                 dat_cell <- raster::values(dat_cell)
                                 dat_cell <- stats::na.omit(dat_cell)
                                 
                                 value <- agg_fun(dat_cell, ...)
                               }, 
                               fine_dat, agg_fun, ...)

  if(raster_output) {
    raster::values(ras) <- matrix(out, 
                                  nrow = dim(ras)[1], 
                                  ncol = dim(ras)[2], 
                                  byrow = TRUE)
    out <- ras
  }
  
  return(out)
}
