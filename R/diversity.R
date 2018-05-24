#'Calculate diversity index for a given input and list of land-cover classes
#'
#'This function calculates a diversity index for an input vector of values or raster based on a list of landcover classes.
#'@param dat The raster dataset on which to calculate the diversity index
#'@param lc_class The list of land cover classes to include in the diversity index calculation
#'@return numeric. Diversity index based on the given landcover classes
#'@keywords Shannon, Simpson, richness, diversity, evenness, focal
#'@export

diversity <- function(dat, lc_class, index = "shei", na.rm = TRUE) {
  if(class(dat) == "RasterLayer") {
    dat <- raster::values(dat)
  }
  
  area <- length(dat)
  
  # check the input index is correct
  indices <- c("pr", "prd", "rpr", "shdi", "sidi", "msidi", "shei", "siei", "msiei")
  match.arg(index, indices)
  
  # main body of function
  if(any(lc_class %in% raster::unique(dat))) {
    # patch richness metrics
    if(index %in% c("pr", "prd", "rpr")) {
      H <- unique(dat)
      if(index == "prd") {
        H <- H / area
      }
      if(index == "rpr") {
        H <- H / length(lc_class)
      }
    }
    
    # more complex diversity metrics
    if (index %in% c("shdi", "sidi", "msidi", "shei", "siei", "msiei")) {
      p <- table(dat) / area
      p <- p[as.character(lc_class)]
      
      if(index %in% c("shdi", "shei")) {
        x <- -p * log(p, exp(1))
      } else {
        x = p * p
      }
      H <- sum(x, na.rm = na.rm)
      if(index == "sidi") {
        H <- 1 - H
      }
      if(index == "msidi") {
        H <- -log(H, exp(1))
      }
      if(index == "shei") {
        H <- H/log(length(p), exp(1))
      }
      if(index == "siei") {
        H <- H/(1 - 1/length(p))
      }
      if(index == "msiei") {
        H <- -log(H, exp(1))/(log(length(p), exp(1)))
      }
    }
    return (H)
  } else {
    return(0)
  }
}

