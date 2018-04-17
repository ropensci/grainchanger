#'Calculate Shannon evenness for a given raster and list of landcover classes
#'
#'This function calculates a value for Shannon evenness for an input raster based on a list of landcover classes. 
#'@param dat The raster dataset on which to calculate proportion
#'@param lc_class The list of land cover classes for which proportion should be calculated. 
#'@return numeric. Proportion of specified land cover classes in the raster
#'@keywords composition, proportion, focal
#'@export
prop <- function(dat, lc_class) {
  prop <- 0
  for(i in lc_class) {
    prop <- prop + sum(raster::as.matrix(dat == i)) / raster::ncell(dat)   
  }
  
  return(prop)
}