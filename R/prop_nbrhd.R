#'Calculate proportion of a given land cover type within a given neighbourhood
#'
#'This function calculates for each cell of a raster the proportion of a given land-cover type within a specified neighbourhood.
#'@param dat The raster dataset on which to calculate the land-cover proportion
#'@param nbrhd The neighbourhood on which to perform the calculation. Defined by raster::focalWeight(). If this is NULL, the calculation is performed on the full grid cell.
#'@param lc_class The land-cover class for calculating the proportion
#'@return raster with proportion of given land-cover in the neighbourhood surrounding each cell
#'@keywords proportion, composition, focal
#'@export

prop_nbrhd <- function (dat, nbrhd, lc_class) 
{
  prop <- raster::focal(dat==i, nbrhd)
  return(prop)
}