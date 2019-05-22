#' Create moving window surface
#'
#' Smooth a raster surface using a moving window with a given function, radius and shape.
#'
#' @param fine_dat The raster dataset on which to calculate the moving window function
#' @param d numeric. If \code{type=circle}, the radius of the circle (in units of the CRS). If
#'  \code{type=rectangle} the dimension of the rectangle (one or two numbers). If \code{type=Gauss}
#'  the size of sigma, and optionally another number to determine the size of the matrix
#'  returned (default is 3 times sigma)
#' @param type The shape of the moving window
#' @param win_fun The function to apply. The function fun should take multiple numbers, and
#'  return a single number. For example mean, modal, min or max. It should also accept a
#'  na.rm argument (or ignore it, e.g. as one of the 'dots' arguments. For example, length
#'  will fail, but function(x, ...){na.omit(length(x))} works. See Details
#' @param ... further arguments passed to or from other methods
#'
#' @return A smoothed raster with the moving window values calculated
#'
#' @keywords focal, spatial
#'
#' @details \code{grainchanger} has several built-in functions. Functions currently
#'  included are:
#'  \itemize{
#'   \item \code{wm_shei} - Shannon evenness, requires the additional argument \code{lc_class} (vector or scalar)
#'   \item \code{wm_prop} - Proportion, requires the additional argument \code{lc_class} (scalar)
#'   \item \code{wm_classes} - Unique number of classes in a categorical landscape
#'   \item \code{var_range} - Range (max - min)
#' }
#'
#' @examples
#' # load required data
#' data(cat_ls)
#' data(cont_ls)
#' 
#' # calculate the moving window mean
#' d <- winmove(cont_ls, 5, "rectangle", "mean")
#' 
#' # calculate the moving window Shannon evenness
#' d <- winmove(cat_ls, 5, "rectangle", "shei", lc_class = 0:3)
#' @export

winmove <- function(fine_dat, d, type, win_fun, ...) {
  
  checkmate::assert_class(fine_dat, "RasterLayer")
  checkmate::assert_numeric(d)

  if (win_fun == "prop") {
    out <- wm_prop(fine_dat, d, type, ...)
    return(out)
  }

  if (win_fun == "mean") {
    out <- wm_mean(fine_dat, d, type, ...)
    return(out)
  }

  if (win_fun == "shei") {
    out <- wm_shei(fine_dat, d, type, ...)
    return(out)
  }

  # this catches all others (i.e. in-built or user-defined functions)
  w <- raster::focalWeight(fine_dat, d, type = type)
  w <- ifelse(w > 0, 1, NA)
  out <- raster::focal(fine_dat, w, function(x) {
    get(win_fun)(x, na.rm = TRUE, ...)
  })
  return(out)
}
