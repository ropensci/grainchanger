#' Moving-window data aggregation
#'
#' Calculate the mean moving window value for a given radius, shape and function
#' for each cell in a larger resolution grid.
#'
#' @param coarse_dat sf, Raster* or Spatial* object. The coarse grain data
#'   (response data) across which to calculate the aggregated moving window
#'   function
#' @param fine_dat Raster* object. The fine grain data (predictor / covariate
#'   data) to aggregate
#' @param d numeric. If \code{type=circle}, the radius of the circle (in units
#'   of the CRS). If \code{type=rectangle} the dimension of the rectangle (one
#'   or two numbers).
#' @param type character. The shape of the moving window
#' @param win_fun character. The function to apply to the moving window. The
#'   function \code{win_fun} should take multiple numbers, and return a single number. For
#'   example \code{mean}, \code{modal}, \code{min} or \code{max}. It should also accept a \code{na.rm} argument (or
#'   ignore it, e.g. as one of the 'dots' arguments. For example, \code{length} will
#'   fail, but \code{function(x, ...){na.omit(length(x))}} works. See Details
#' @param agg_fun character. The function by which to aggregate. By default this
#'   is set to \code{mean}
#' @param is_grid logical. Use \code{TRUE} (default) if \code{g} contains only
#'   rectangular cells (i.e. a grid). If \code{g} is any other polygon file,
#'   this should be set to false
#' @param quiet logical. If \code{FALSE} (default) and \code{is_grid == TRUE}
#'   the user gets a warning that the aggregation assumes all cells are
#'   rectangular
#' @param ... further arguments passed to or from other methods
#'
#' @return Numeric vector containing moving window values calculated for each
#'   grid cell
#'
#' @keywords focal, spatial, aggregate
#'
#' @details \code{grainchanger} has several built-in functions. Functions
#'   currently included are: 
#'   \itemize{ 
#'      \item \code{shdi} - Shannon diversity, requires the additional argument \code{lc_class} (vector or scalar) 
#'      \item \code{shei} - Shannon evenness, requires the additional argument \code{lc_class} (vector or scalar) 
#'      \item \code{prop} - Proportion, requires the additional argument \code{lc_class} (scalar)
#'      \item \code{var_range} - Range (max - min) 
#'      }
#'      
#'  Note that \code{winmove_agg} can be run in parallel using \code{plan(multiprocess)} from the \code{future} package. 
#' @examples
#' # load required data
#' data(g_sf)
#' data(cont_ls)
#' data(cat_ls)
#'
#' # aggregate using mean
#' d <- winmove_agg(g_sf, cont_ls, 5, "rectangle", mean)
#'
#' # aggregate using Shannon evenness
#' d <- winmove_agg(g_sf, cat_ls, 5, "rectangle", shei, lc_class = 1:4)
#' @export

winmove_agg <- function(coarse_dat, 
                        fine_dat, 
                        d, 
                        type=c('circle', 'rectangle'), 
                        win_fun, 
                        agg_fun = mean, 
                        is_grid = TRUE, 
                        quiet = FALSE,
                        ...) {
  
  checkmate::assert(
    checkmate::check_class(coarse_dat, "RasterLayer"),
    checkmate::check_class(coarse_dat, "SpatialPolygonsDataFrame"),
    checkmate::check_class(coarse_dat, "SpatialPolygons"),
    checkmate::check_class(coarse_dat, "sf"),
    checkmate::check_class(coarse_dat, "sfc")
  )

  checkmate::assert_class(fine_dat, "RasterLayer")
  checkmate::assert_numeric(d)
  

  if(is_grid & !quiet) {
    usethis::ui_line("aggregation assumes all cells are rectangular")
    usethis::ui_todo("set `is_grid = FALSE` if coarse_dat is not a grid")
  }
  
  # convert raster to grid
  output_raster <- FALSE
  if ("RasterLayer" %in% class(coarse_dat)) {
    output_raster <- TRUE
    ras <- coarse_dat
    raster::values(ras) <- 1
    coarse_dat <- methods::as(ras, "SpatialPolygonsDataFrame")
  } 

  if ("sp" %in% attr(class(coarse_dat), which = "package")) {
    coarse_dat <- sf::st_as_sf(coarse_dat)
  }

  if(!quiet) {
    buff <- sf::st_buffer(coarse_dat, d)
    dat_bbox <- sf::st_bbox(fine_dat)
    contained <- unlist(sf::st_contains(sf::st_as_sfc(dat_bbox), buff))
    affected <- paste0(rownames(buff[-contained,]), collapse = ",")
    if(length(affected) > 0) {
      usethis::ui_warn(
        paste0(
          "Moving window extends beyond extent of `fine_dat`
          You will get edge effects for the following cells of `coarse_dat`:
          ",affected
          )
        )
    }
  }
  out <- furrr::future_map(sf::st_geometry(coarse_dat), 
                           purrr::quietly(function(grid_cell, 
                                                   fine_dat, 
                                                   d, 
                                                   type, 
                                                   win_fun, 
                                                   agg_fun,
                                                   ...) {
                             
                             # buffer and crop
                             grid_buffer <- sf::st_sf(
                               sf::st_geometry(
                                 sf::st_buffer(grid_cell, 
                                               dist = d, 
                                               endCapStyle = "SQUARE", 
                                               joinStyle = "MITRE", 
                                               mitreLimit = d / 2)
                               )
                             )
                             
                             dat_cell <- raster::crop(fine_dat, 
                                                      grid_buffer)
                             dat_cell <- raster::extend(dat_cell, 
                                                        grid_buffer)
                               
                             # mask if coarse_dat not rectangular
                             if(!is_grid) {
                               dat_cell <- raster::mask(dat_cell, 
                                                        grid_buffer)
                             }
                             win_cell <- winmove(dat_cell, 
                                                 d, 
                                                 type, 
                                                 win_fun, 
                                                 ...)
                             agg_fun(raster::values(win_cell),
                                                    na.rm = TRUE)
                           }), fine_dat, d, type, win_fun, agg_fun, ...)

  out <- furrr::future_map_dbl(out, function(x) x$result)
  
  
  if(output_raster) {
    raster::values(ras) <- matrix(out, 
                                  nrow = dim(ras)[1], 
                                  ncol = dim(ras)[2], 
                                  byrow = TRUE)
    out <- ras
  }
  return(out)
}
