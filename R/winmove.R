#' Create moving window surface
#'
#' Smooth a raster surface using a moving window with a given function, radius and shape.
#'
#' @param fine_dat The raster dataset on which to calculate the moving window function
#' @param d numeric. If \code{type=circle}, the radius of the circle (in units of the
#'   CRS). If \code{type=rectangle} the dimension of the rectangle (one or two numbers).
#'   If \code{type=Gauss} the size of sigma, and optionally another number to determine
#'   the size of the matrix returned (default is 3 times sigma)
#' @param type The shape of the moving window
#' @param win_fun function. The function to apply. If not choosing one of the inbuilt
#'   grainchanger functions, the function should take multiple numbers, and return a
#'   single number. For example mean, modal, min or max. It should also accept a na.rm
#'   argument (or ignore it, e.g. as one of the 'dots' arguments. For example, length will
#'   fail, but function(x, ...){na.omit(length(x))} works. See Details
#' @param ... further arguments passed to or from other methods
#'
#' @return RasterLayer. A smoothed raster with the moving window values calculated
#'
#' @keywords focal spatial
#'
#' @details \code{grainchanger} has several built-in functions. Functions currently
#'   included are: \itemize{ \item \code{wm_shei} - Shannon evenness, requires the
#'   additional argument \code{lc_class} (vector or scalar) \item \code{wm_prop} -
#'   Proportion, requires the additional argument \code{lc_class} (scalar) \item
#'   \code{wm_classes} - Unique number of classes in a categorical landscape \item
#'   \code{var_range} - Range (max - min) }
#'
#' @examples
#' # load required data
#' data(cat_ls)
#' data(cont_ls)
#'
#' # calculate the moving window mean
#' d <- winmove(cont_ls, 5, "rectangle", mean)
#'
#' # calculate the moving window Shannon evenness
#' d <- winmove(cat_ls, 5, "rectangle", shei, lc_class = 1:4)
#' @export

winmove <- function(fine_dat, d, type, win_fun, ...) {
  
  checkmate::assert_class(fine_dat, "RasterLayer")
  checkmate::assert_numeric(d)
  
  # for grainchanger in-built functions
  if("grainchanger" %in% environment(win_fun)$.packageName) {
    # set fine_dat to the winmove class so it picks up the inbuilt functions
    fine_dat <- methods::new("winmove", fine_dat)
    out <- win_fun(fine_dat, d, type, ...)
  } else {
    # this catches all others (i.e. base R in-built or user-defined functions)
    w <- raster::focalWeight(fine_dat, d, type = type)
    w <- ifelse(w > 0, 1, NA)
    out <- raster::focal(fine_dat, w, function(x) {
      win_fun(x, na.rm = TRUE, ...)
    })
  }
  return(out)
}

#' An S4 class for use with winmove functions (extends RasterLayer)
#' @description An S4 class for use with winmove functions (extends RasterLayer). Objects
#'   will need to be set to this class in order to be used with the inbuilt \code{winmove}
#'   functions (e.g. \code{mean}, \code{prop}, \code{var_range}, \code{shdi}, \code{shei})
#'   
#' @inheritSection raster::`RasterLayer-class` Slots
#' 
#' @importClassesFrom raster RasterLayer
#' 
#' @export
#' 
#' @examples 
#' # load required data
#' data(cat_ls)
#' 
#' # set \code{cat_ls} to object of class winmove
#' new("winmove", cat_ls)
#' 
setClass("winmove", contains = "RasterLayer")