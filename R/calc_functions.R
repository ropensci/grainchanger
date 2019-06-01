#' Arithmetic mean
#'
#' An extension to \code{mean} for objects of class \code{winmove}
#'
#' @param x RasterLayer. The data over which to calculate the mean value within a moving
#'   window
#' @param d numeric. If \code{type=circle}, the radius of the circle (in units of the
#'   CRS). If \code{type=rectangle} the dimension of the rectangle (one or two numbers)
#' @param type character. The shape of the moving window
#' @param ... further arguments passed to or from other methods
#' 
#' @return RasterLayer. A smoothed raster with the mean calculated within the specified
#'   moving window
#'   
#' @keywords focal spatial mean
#' 
#' @examples 
#' 
#' # load required data
#' data(cont_ls)
#' 
#' # convert data to object of class winmove
#' cont_ls <- new("winmove", cont_ls)
#' 
#' # aggregate using a circular window with radius 3
#' d <- mean(cont_ls, d = 3, type = "circle")
#' @export
mean <- function(x, ...) UseMethod("mean")

#' @name mean
#' @export
mean.winmove <- function(x, d, type, ...) {
  w <- raster::focalWeight(x, d, type)
  return(raster::focal(x, w))
}

#' Size of range of values
#' 
#' @description Calculates the difference between the maximum and minimum value
#' 
#' @param x RasterLayer. The data over which to calculate the range size
#' @param d numeric. If \code{type=circle}, the radius of the circle (in units of the
#'   CRS). If \code{type=rectangle} the dimension of the rectangle (one or two numbers)
#' @param type character. The shape of the moving window
#' @param na.rm logical. indicates whether \code{NA} values should be stripped before the
#'   computation proceeds. \code{na.rm = TRUE} is the default
#' @param ... further arguments passed to or from other methods
#'   
#' @return If \code{class(x) == "winmove"}, a smoothed raster with the size of the range of values calculated within the specified
#'   moving window
#'   
#'   If \code{class(x) == "numeric"}, a single value representing the size of the range of values in \code{x}
#'   
#' @keywords focal spatial range
#' 
#' @examples 
#' 
#' # load required data
#' data(cat_ls)
#' data(cont_ls)
#' 
#' # convert data to object of class winmove
#' cat_ls <- new("winmove", cat_ls)
#' 
#' # aggregate using a rectangular window with dimensions c(2,3)
#' d <- range(cont_ls, d = c(2,3), type = "rectangle")
#' 
#' # convert data to object of class numeric
#' cont_ls <- raster::values(cont_ls)
#' d <- range(cont_ls)
#' 
#' @export
var_range <- function(x, ...) UseMethod("var_range")

#' @name var_range
#' @export
var_range.winmove <- function(x, d, type, na.rm = TRUE, ...) {
  w <- ifelse(raster::focalWeight(x, d, type) == 0, 0, 1)
  return(raster::focal(x = x, w = w, fun = function(y) {
    if(all(is.na(y))) {
      NA
    } else {
      max(y, na.rm = na.rm) - min(y, na.rm = na.rm)  
    }
    
  }))
}

#' @name var_range
#' @export
var_range.numeric <- function(x, na.rm = TRUE, ...) {
  if(all(is.na(x))) {
    return(NA)
  } else {
    return(max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
  }
}

#' Calculate proportion of a given value
#'
#' Calculate the proportion of a given value present within a raster. Useful for
#' calculating land-cover or soil type proportions. Shoudl be used with a categorical
#' raster
#'
#' @param x numeric, winmove. The data over which to calculate the proportion
#' @param lc_class numeric. The class value to calculate the proportion of
#' @param d numeric. If \code{type=circle}, the radius of the circle (in units of the
#'   CRS). If \code{type=rectangle} the dimension of the rectangle (one or two numbers)
#' @param type character. The shape of the moving window
#' @param ... further arguments passed to or from other methods
#'
#' @return If \code{class(x) == "winmove"}, a smoothed raster with the proportion of
#'   cells of the given class calculated within the specified moving window
#'
#'   If \code{class(x) == "numeric"}, a single value representing the proportion of values
#'   of a given class in \code{x}
#'
#' @keywords focal spatial mean
#'
#' @examples
#'
#' # load required data
#' data(cat_ls)
#'
#' # convert data to object of class winmove
#' cat_ls <- new("winmove", cat_ls)
#'
#' # aggregate using a rectangular window with dimension 5 for class 3
#' d <- prop(cat_ls, d = 5, type = "rectangle", lc_class = 3)
#' 
#' # convert data to object of class numeric
#' cat_ls <- raster::values(cat_ls)
#' d <- prop(cat_ls, lc_class = 2)
#' @export
prop <- function(x, lc_class, ...) UseMethod("prop")

#' @name prop
#' @export
prop.winmove <- function(x, lc_class, d, type, ...) {
  if(sum(raster::values(x) == lc_class, na.rm = TRUE) > 0) {
    return(raster::focal(x == lc_class,
                         raster::focalWeight(x, d, type)))
  }
  else {
    raster::values(x) <- 0
    return(x)
  }
}

#' @name prop
#' @export
prop.numeric <- function(x, lc_class, ...) {
  area <- length(x)
  p <- sum(x %in% lc_class) / area
  return(p)
}

#' Diversity metrics
#' @name diversity-metrics
#' @description A range of functions to calculate well known landcover diversity metrics
#'
#' @param x numeric, winmove. The data over which to calculate the diversity metrics
#' @param lc_class numeric. The class values to include in the diversity metric
#'   calculation
#' @param d numeric. If \code{type=circle}, the radius of the circle (in units of the
#'   CRS). If \code{type=rectangle} the dimension of the rectangle (one or two numbers)
#' @param type character. The shape of the moving window
#' @param ... further arguments passed to or from other methods
#' 
#' @return If \code{class(x) == "winmove"}, a smoothed raster with the diversity
#'   metric calculated within the specified moving window
#'
#'   If \code{class(x) == "numeric"}, a single value representing the diversity metric in
#'   \code{x}
#'
#' @details Currently provided diversity metrics are Shannon diversity and Shannon
#'   evenness. Open a new issue (https://github.com/laurajanegraham/grainchanger/issues)
#'   to request additional diversity metrics.
#'
#' @references McGarigal, K. and Marks, B.J., 1995. FRAGSTATS: spatial pattern analysis
#'   program for quantifying landscape structure. \emph{Gen. Tech. Rep. PNW-GTR-351. Portland,
#'   OR: US Department of Agriculture, Forest Service, Pacific Northwest Research Station.
#'   122 p, 351.}
#'   
#' @examples
#' # load required data
#' data(cat_ls)
#'
#' # convert data to object of class winmove
#' cat_ls <- new("winmove", cat_ls)
#'
#' # calculate Shannon diversity in a rectangular window of dimension 5
#' d <- shdi(cat_ls, d = 5, type = "rectangle", lc_class = 1:4)
#'
#' # convert data to object of class numeric
#' cat_ls <- raster::values(cat_ls)
#'
#' # calculate Shannon evenness
#' d <- shei(cat_ls, lc_class = 1:4)
NULL

#' @noRd
#' @export
shdi <- function(x, lc_class, ...) UseMethod("shdi")

#' @name shdi
#' @rdname diversity-metrics
#' @export
shdi.winmove <- function(x, lc_class, d, type, ...) {
  H <- lapply(lc_class, function(i) {
    p <- prop(x = x, lc_class = i, d = d, type = type)
    -1 * p * log(p)
  })
  return(sum(raster::stack(H), na.rm = TRUE))
}

#' @name shdi
#' @rdname diversity-metrics
#' @export
shdi.numeric <- function(x, lc_class, ...) {
  dat <- stats::na.omit(x)
  H <- lapply(lc_class, function(i) {
    p <- sum(x %in% i) / length(x)
    -1 * p * log(p)
  })
  return(sum(unlist(H), na.rm = TRUE))
}

#' @noRd
#' @export
shei <- function(x, lc_class, ...) UseMethod("shei")

#' @name shei
#' @rdname diversity-metrics
#' @export
shei.winmove <- function(x, lc_class, d, type, ...) {
  shdi(x = x, lc_class = lc_class, d = d, type = type) / log(length(lc_class))
}

#' @name shei
#' @rdname diversity-metrics
#' @export
shei.numeric <- function(x, lc_class, ...) {
  shdi(x = x, lc_class = lc_class) / log(length(lc_class))
}