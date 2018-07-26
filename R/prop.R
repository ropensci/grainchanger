#'Proportion of landscape
#'
#'This function calculates the total proportion of the landscape covered by a given land cover type(s)
#'@param dat A raster or vector
#'@param lc_class The list of land cover classes for which proportion should be calculated. 
#'@return numeric. Proportion of specified land cover classes in the raster
#'@keywords composition, proportion, focal
#'@export
prop <- function(dat, lc_class, na.rm = TRUE) {
  if(class(dat) == "RasterLayer") {
    dat <- raster::values(dat)
  }
  
  area <- length(dat)
  p <- table(dat) / area
  p <- p[as.character(lc_class)]
  p <- sum(p, na.rm = na.rm)
  
  return(p)
}
