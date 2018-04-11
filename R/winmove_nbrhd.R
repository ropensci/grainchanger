#'Calculate the value of a given function for each cell, within a given neighbourhood. 
#'
#'This function takes as input a raster dataset, the radius and type of moving window (`circle` or `rectangle` ) and a function to apply across the moving window. A new raster with the moving window value for each cell is then output.
#'@param dat The raster dataset on which to calculate the moving window function
#'@param radius The radius of the moving window
#'@param type The shape of the moving window
#'@param fn The function to apply - inbuilt functions are currently `diversity` (Shannon evenness of a given list of land-covers) and `prop` (proportion of a given land-cover). For inbuilt functions, land-cover should be passed in as an argument `lc_class`
#'@param ... further arguments passed to or from other methods (`lc_class` as a list for `diversity` or integer for `prop`)
#'@return A raster with the moving window values calculated
#'@export

winmove_nbrhd <- function(dat, radius, type=c("circle", "rectangle"), fn, ...) {
  wdw <- raster::focalWeight(dat, radius, type=type)
  if(fn == "diversity") {
    out <- diversity_nbrhd(dat, wdw, ...)
  } else if(fn == "mean") {
    out <- raster::focal(dat, wdw)
  } else if(fn == "prop") {
    out <- prop_nbrhd(dat, wdw, ...)
  } else {
    # for all other functions, we just want weights of 1
    wdw <- ifelse(wdw > 0, 1, NA)
    out <- raster::focal(dat, wdw, get(fn), na.rm = TRUE, ...)
  }
  return(out)
}
