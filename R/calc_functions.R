# proportion
wm_prop <- function(dat, d, type, i) {
  w <- raster::focalWeight(dat, d, type)
  raster::focal(dat == i, w)
}

# unique
wm_classes <- function(dat, d, type) {
  w <- ifelse(raster::focalWeight(dat, d, type) == 0, 0, 1)
  raster::focal(dat, w, function(x) length(raster::unique(x)))
}

# diversity metrics
wm_shei <- function(dat, d, type, lc_class) {
  H <- lapply(lc_class, function(i) {
    p <- wm_prop(dat, d, type, i)
    -1*p*log(p)
  })
  sum(raster::stack(H))/log(length(lc_class))
}

# mean (it's quicker using weighted sum than mean)
wm_mean <- function(dat, d, type) {
  w <- raster::focalWeight(dat, d, type)
  raster::focal(dat, w)
}
