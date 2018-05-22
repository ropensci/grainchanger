#'Calculate Shannon evenness for a given raster and list of landcover classes
#'
#'This function calculates a value for Shannon evenness for an input raster based on a list of landcover classes. 
#'@param dat The raster dataset on which to calculate Shannon evenness
#'@param lc_class The list of land cover classes for which Shannon evenness should be calculated. 
#'@return numeric. Shannon evenness for the raster based on the given landcover classes
#'@keywords Shannon, diversity, evenness, focal
#'@export
diversity <- function(dat, lc_class) {
  area <- raster::ncell(dat)
  
  if(any(lc_class %in% raster::unique(dat))) {
    p <- dat %>%
      raster::values() %>%
      table() / area
    p <- p[as.character(lc_class)]
    
    H <- sum(-p * log(p, exp(1)), na.rm = TRUE) / log(length(p), exp(1))
    return (H)
  } else {
    return(0)
  }
}