# Mean (winmove only) ----
#' @export
mean <- function(x, ...) UseMethod("mean")

#' @name mean
#' @export
mean.winmove <- function(x, d, type, na.rm = FALSE) {
  if(na.rm) {
    w <- ifelse(raster::focalWeight(x, d, type) == 0, 0, 1)
    return(raster::focal(x = x, w = w, fun = mean, na.rm = TRUE))
  } else {
    w <- raster::focalWeight(x, d, type)
    return(raster::focal(x, w))
  }
}

# Range ----
#' @export
var_range <- function(x, ...) UseMethod("var_range")

#' @name var_range
#' @export
var_range.winmove <- function(x, d, type, na.rm = TRUE) {
  w <- ifelse(raster::focalWeight(x, d, type) == 0, 0, 1)
  return(raster::focal(x = x, w = w, fun = function(dat) {
    max(dat, na.rm = na.rm) - min(dat, na.rm = na.rm)
  }))
}

#' @name var_range
#' @export
var_range.nomove <- function(dat, na.rm = TRUE) {
  return(max(dat, na.rm = na.rm) - min(dat, na.rm = na.rm))
}

# Proportion ----
#' @export
prop <- function(x, lc_class, ...) UseMethod("prop")

#' @name prop
#' @export
prop.winmove <- function(dat, d, type, lc_class) {
  if(sum(raster::values(dat) == lc_class, na.rm = TRUE) > 0) {
    return(raster::focal(dat == lc_class,
                         raster::focalWeight(dat, d, type)))
  }
  else {
    return(0)
  }
}

#' @name prop
#' @export
prop.nomove <- function(dat, lc_class) {
  area <- length(dat)
  p <- sum(dat %in% lc_class) / area
  return(p)
}

# Shannon diversity ----
#' @export
shdi <- function(x, lc_class, ...) UseMethod("shdi")

#' @name shdi
#' @export
shdi.winmove <- function(x, lc_class, d, type) {
  H <- lapply(lc_class, function(i) {
    p <- prop(x, d, type, i)
    -1 * p * log(p)
  })
  sum(raster::stack(H), na.rm = TRUE)
}

#' @name shdi
#' @export
shdi.nomove <- function(x, lc_class) {
  dat <- stats::na.omit(x)
  H <- lapply(lc_class, function(i) {
    p <- sum(x == i) / raster::ncell(x)
    -1 * p * log(p)
  })
  sum(unlist(H), na.rm = TRUE)
}

# Shannon evenness ----
#' @export
shei <- function(x, lc_class, ...) UseMethod("shei")

#' @name shei
#' @export
shei.winmove <- function(x, lc_class, d, type) {
  shdi(x, lc_class, d, type) / log(length(lc_class))
}

#' @name shei
#' @export
shei.nomove <- function(x, lc_class) {
  shdi(x, lc_class) / log(length(lc_class))
}