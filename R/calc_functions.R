#' @noRd
# proportion
wm_prop <- function(dat, d, type, lc_class) {
  if(sum(raster::values(dat), na.rm = TRUE) > 0) {
    return(raster::focal(dat == lc_class,
                         raster::focalWeight(dat, d, type)))
  }
  else {
    return(0)
  }
}

#' @noRd
# unique
wm_classes <- function(dat, d, type) {
  w <- ifelse(raster::focalWeight(dat, d, type) == 0, 0, 1)
  raster::focal(dat, w, function(x) length(raster::unique(x)))
}

#' @noRd
# diversity metrics
wm_shei <- function(dat, d, type, lc_class) {
  H <- lapply(lc_class, function(i) {
    p <- wm_prop(dat, d, type, i)
    -1 * p * log(p)
  })
  sum(raster::stack(H), na.rm = TRUE) / log(length(lc_class))
}

#' @noRd
# mean (it's quicker using weighted sum than mean, but mean needed if na.rm required)
wm_mean <- function(dat, d, type, ...) {
  args <- list(...)
  exist <- "na.rm" %in% names(args)
  if(!exist) na.rm = FALSE
  
  if(na.rm) {
    w <- ifelse(raster::focalWeight(dat, d, type) == 0, 0, 1)
    return(raster::focal(x = dat, w = w, fun = mean, na.rm = TRUE))
  } else {
    w <- raster::focalWeight(dat, d, type)
    return(raster::focal(dat, w))
  }
}

#' @noRd
nm_shei <- function(dat, lc_class) {
  dat <- stats::na.omit(dat)
  H <- lapply(lc_class, function(i) {
    p <- sum(dat == i) / raster::ncell(dat)
    -1 * p * log(p)
  })
  sum(unlist(H), na.rm = TRUE) / log(length(lc_class))
}

#' @noRd
nm_prop <- function(dat, lc_class) {
  if (class(dat) == "RasterLayer") {
    dat <- raster::values(dat)
  }
  
  area <- length(dat)
  p <- sum(dat %in% lc_class) / area
  return(p)
}

#' @noRd
var_range <- function(dat, na.rm = TRUE) {
  if (class(dat) == "RasterLayer") {
    dat <- raster::values(dat)
  }

  if (sum(is.na(dat)) == length(dat)) {
    return(NA)
  } else {
    var_range <- max(dat, na.rm = na.rm) - min(dat, na.rm = na.rm)
    return(var_range)
  }
}
