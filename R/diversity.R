#'Calculate Shannon evenness for a given raster and list of landcover classes
#'
#'This function calculates a value for Shannon evenness for an input raster based on a list of landcover classes. 
#'@param dat The raster dataset on which to calculate Shannon evenness
#'@param lc_class The list of land cover classes for which Shannon evenness should be calculated. 
#'@return numeric. Shannon evenness for the raster based on the given landcover classes
#'@keywords Shannon, diversity, evenness, focal
#'@export
diversity <- function(dat, lc_class) {
  if(any(lc_class %in% raster::unique(dat))) {
    entropy.r <- plyr::ldply(lc_class, function(i) {
      prop_class <- sum(as.vector(dat == i)) / raster::ncell(dat) 
      entropy <- xlog(prop_class)
    })
    entropy.r <- sum(entropy.r)
    shannon <- (0 - entropy.r/log(length(lc_class)))
    return (shannon)
  } else {
    return(0)
  }
}