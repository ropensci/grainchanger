#'Calculate the value of a given function for each cell, within a given neighbourhood. 
#'
#'This function takes as input a raster dataset, the radius and type of moving window (`circle` or `rectangle` ) and a function to apply across the moving window. A new raster with the moving window value for each cell is then output.
#'@param dat The raster dataset on which to calculate the moving window function
#'@param radius The radius of the moving window
#'@param type The shape of the moving window
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return A raster with the moving window values calculated
#'@export

winmove_nbrhd <- function(dat, radius, type=c("circle", "rectangle"), verbose = FALSE, fn, ...) {
  res_dat <- raster::res(dat)
  dims_cells <- floor(radius/res_dat)*2 + 1
  dims_units <- res_dat*((dims_cells -1)/2)
  # the reason for putting this in is that the radius that goes in isn't
  # necessarily the one that comes out due to the need for the radius to be an
  # exact multiple of the resolution
  if(verbose) {
    print("Moving window radius: ")
    print(dims_units)
  }
  
  wdw <- raster::focalWeight(dat, radius, type=type)
  if(fn == "diversity") {
    out <- diversity_nbrhd(dat, wdw, ...)
  } else if(fn == "mean") {
    out <- raster::focal(dat, wdw)
  } else {
    # for all other functions, we just want weights of 1
    wdw <- ifelse(wdw > 0, 1, NA)
    out <- raster::focal(dat, wdw, get(fn), na.rm = TRUE, ...)
  }
  return(out)
}
