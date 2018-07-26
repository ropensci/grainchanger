#'Variable range
#'
#'This function calculates the range of a continuous variable
#'@param dat A raster or vector
#'@return numeric. Range of the variable
#'@keywords range, focal
#'@export
var_range <- function(dat, na.rm = TRUE) {
  if(class(dat) == "RasterLayer") {
    dat <- raster::values(dat)
  }
  
  if(sum(is.na(dat)) == length(dat)) {
    return(NA)
  } else {
    var_range <- max(dat, na.rm = na.rm) - min(dat, na.rm = na.rm)
    return(var_range)
  }
}
