#'Calculate Shannon evenness for a given neighbourhood and list of landcover classes
#'
#'This function calculates a value for Shannon evenness for each cell of a raster within a specified neighbourhood based on a list of landcover classes. 
#'@param dat The raster dataset on which to calculate the moving window Shannon evenness
#'@param nbrhd The neighbourhood on which to perform the calculation. Defined by raster::focalWeight(). If this is NULL, the calculation is performed on the full grid cell.
#'@param lc_class The list of land cover classes for which Shannon evenness should be calculated. 
#'@return raster with Shannon evenness value for the neighbourhood surrounding each cell
#'@keywords Shannon, diversity, evenness, focal
#'@export

diversity_nbrhd <- function (dat, nbrhd, lc_class) 
{
  if (any(lc_class %in% raster::unique(dat))) {
    entropy.r <- lapply(lc_class, function(i) {
      raster::calc(raster::focal(dat == i, nbrhd), xlog)
    })
    shannon <- (0 - sum(raster::stack(entropy.r)))/log(length(lc_class))
    return(shannon)
  }
  else {
    return(0)
  }
}