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
      raster::calc(raster::focal(dat == i, nbrhd), x.log)
    })
    shannon <- (0 - sum(raster::stack(entropy.r)))/log(length(lc_class))
    return(shannon)
  }
  else {
    return(0)
  }
}

#'Calculate the value of a given function for each cell, within a given neighbourhood. 
#'
#'This function takes as input a raster dataset, the radius and type of moving window (`circle`, `rectangle` or `Gauss`) and a function to apply across the moving window. A new raster with the moving window value for each cell is then output.
#'@param dat The raster dataset on which to calculate the moving window function
#'@param radius The radius of the moving window
#'@param type The shape of the moving window
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return A raster with the moving window values calculated
#'@export

winmove_nbrhd <- function(dat, radius, type=c("circle", "Gauss", "rectangle"), fn, ...) {
  nbrhd <- raster::focalWeight(dat, radius, type=type)
  
  if(fn == "diversity") {
    out <- diversity_nbrhd(dat, nbrhd, ...)
  } else {
    out <- raster::focal(dat, nbrhd, get(fn), ...)
  }
  return(out)
}

#'Calculate the value of a given function for each cell, within a given neighbourhood, for a cell in a larger resolution grid. 
#'
#'This function takes as input the row identifier of a cell within a large resolution grid and calculates the moving window function for a smaller resolution raster dataset using the `winmove_nbrhd()` function.
#'@param cell the row identifier of the grid cell
#'@param grid the grid from which to take the cell
#'@param dat The raster dataset on which to calculate the moving window function
#'@param radius The radius of the moving window
#'@param type The shape of the moving window
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return A raster with the moving window values calculated
#'@export

winmove_cell <- function(cell, grid, dat, radius, type, fn, ...) {
  grid_cell <- grid[cell, ]
  # i'm currently adding a buffer to remove edge effects and actually
  # incorporate the outside effect, this will then be removed. Need to think
  # about how this can be changed to allow for other uses
  # i can do this using pad = T but at the moment not sure I want that functionality available.
  grid_buffer <- rgeos::gBuffer(grid_cell, width = radius, capStyle = "SQUARE", joinStyle = "MITRE", mitreLimit = radius/2)
  dat_cell <- raster::crop(dat, grid_buffer) 
  out <- winmove_nbrhd(dat_cell, radius, type, fn, ...)
  return(out)
}

#' Calculate the mean value of an input raster
#' 
#' This function takes a raster as input and calculates it's mean value. For my purposes, this is used to upscale.
#' @param winmove_cellr raster. 
#' @return numeric. The mean value of the input raster
#' @export

winmove_cellmean <- function(winmove_cellr) {
  out <- mean(raster::as.matrix(winmove_cellr), na.rm=TRUE)
  return(out)
}

#' Upscale the moving window value
#' 
#' Calculate the mean moving window value for a given radius/shape/function for each cell in a larger resolution grid. 
#'@param grid the grid across which to calculate the upscaled moving window function
#'@param dat The raster dataset on which to calculate the moving window function
#'@param radius The radius of the moving window
#'@param type The shape of the moving window
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return Numeric vector containing moving window values calculated for each grid cell
#'@export

winmove_upscale <- function(grid, dat, radius, type, fn, ...) {
  cell_div <- pbapply::pblapply(1:nrow(grid), winmove_cell, grid, dat, radius, type, fn, ...)
  cell_mean <- sapply(cell_div, winmove_cellmean)
  return(cell_mean)
}

# moved out of the diversity function - needs the bumpf for pacakgeing. don't export
x.log <- function(x) ifelse(x==0, 0, x*log(x))




#'Calculate the value of a given function for a raster within a particular cell of a larger grid
#'
#'This function takes as input the row identifier of a cell within a large resolution grid and calculates the value for a given function for a smaller resolution raster dataset. 
#'@param cell the row identifier of the grid cell
#'@param grid the grid from which to take the cell
#'@param dat The raster dataset on which to calculate the function
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return numeric. The value of the function applied to the raster within the larger resolution cell
#'@export
nomove_cell <- function(cell, grid, dat, fn, ...) {
  grid_cell <- grid[cell, ]
  dat_cell <- raster::crop(dat, grid_cell)
  if(fn == "diversity") {
    out <- diversity_cell(dat_cell, ...)
  } else {
    out <- get(fn)(raster::as.matrix(dat_cell), ...)
  }
  return(out)
}

#' Upscale the function value
#' 
#' Calculate the value for a given function for each cell in a larger resolution grid. 
#'@param grid the grid across which to calculate the upscaled moving window function
#'@param dat The raster dataset on which to calculate the moving window function
#'@param fn The function to apply 
#'@param ... further arguments passed to or from other methods
#'@return Numeric vector containing values calculated for each grid cell
#'@export
nomove_upscale <- function(grid, dat, fn, ...) {
  cell_values <- pbapply::pbsapply(1:nrow(grid), nomove_cell, grid, dat, fn, ...)
  return(cell_values)
}

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
      prop_class <- sum(as.matrix(dat == i)) / raster::ncell(dat) 
      entropy <- x.log(prop_class)
    })
    entropy.r <- sum(entropy.r)
    shannon <- (0 - entropy.r/log(length(lc_class)))
    return (shannon)
  } else {
    return(0)
  }
}
  
