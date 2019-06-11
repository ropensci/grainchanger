#' Pad a raster by a specified radius
#'
#' This function pads a raster by a specified number of cells, creating the
#' effect of a torus. This function is intended for use on simulated landscapes,
#' in order to avoid edge effects
#'
#' @param dat The raster dataset to pad
#' @param dpad The amount by which to pad the raster (in the same units as the
#'   raster)
#'
#' @return raster. Original raster padded by r cells with torus effect (see
#'   Details)
#'
#' @keywords torus raster
#'
#' @details A torus is an infinite surface where the top joins the bottom, and
#'   the left side meets the right side. See https://en.wikipedia.org/wiki/Torus
#'   for a full mathematical description.
#'
#'   In this function, the torus effect is achieved by adding the specified
#'   number of rows of the top of the raster to the bottom (and vice versa) and
#'   the specified number of rows of the right of the raster to the left (and
#'   vice versa)
#'   
#' @examples
#' data(cat_ls)
#' d <- create_torus(dat = cat_ls, dpad = 5)
#' @export

create_torus <- function(dat, dpad) {
  
  # This function takes as input a raster and an integer distance value.
  checkmate::assert_class(dat, "RasterLayer")
  checkmate::assert_numeric(dpad)

  resolution <- raster::res(dat)[1]
  dpad <- ceiling(dpad / resolution)

  #   1. Convert raster to matrix
  dat_m <- raster::as.matrix(dat)
  nrows <- nrow(dat_m)
  ncols <- ncol(dat_m)

  #   2. Create new matrix of dim + radius*2
  dat_pad_m <- matrix(NA, 
                      nrow = nrows + 2 * dpad, 
                      ncol = ncols + 2 * dpad)

  #   3. Infill with values from the matrix
  # top
  dat_pad_m[1:dpad, 
            (dpad + 1):(ncols + dpad)] <- 
    dat_m[(nrows - dpad + 1):nrows, ]
  # left
  dat_pad_m[(dpad + 1):(nrows + dpad),
            1:dpad] <- 
    dat_m[, (ncols - dpad + 1):ncols]
  # bottom
  dat_pad_m[(nrows + dpad + 1):(nrows + 2 * dpad), 
            (dpad + 1):(ncols + dpad)] <- 
    dat_m[1:dpad, ]
  # right
  dat_pad_m[(dpad + 1):(nrows + dpad), 
            (ncols + dpad + 1):(ncols + 2 * dpad)] <- 
    dat_m[, 1:dpad]
  # top left corner
  dat_pad_m[1:dpad, 1:dpad] <- 
    dat_m[(nrows - dpad + 1):nrows, 
          (ncols - dpad + 1):ncols]
  # top right corner
  dat_pad_m[1:dpad, 
            (ncols + dpad + 1):(ncols + 2 * dpad)] <- 
    dat_m[(nrows - dpad + 1):nrows, 1:dpad]
  # bottom left corner
  dat_pad_m[(nrows + dpad + 1):(nrows + 2 * dpad), 
            1:dpad] <- 
    dat_m[1:dpad, (ncols - dpad + 1):ncols]
  # bottom right corner
  dat_pad_m[(nrows + dpad + 1):(nrows + 2 * dpad), 
            (ncols + dpad + 1):(ncols + 2 * dpad)] <- 
    dat_m[1:dpad, 1:dpad]
  # centre
  dat_pad_m[(dpad + 1):(nrows + dpad), 
            (dpad + 1):(ncols + dpad)] <- 
    dat_m

  #   4. convert to raster
  dat_pad <- raster::raster(dat_pad_m)

  #   5. Fix resolution
  # specify resolution ----
  raster::extent(dat_pad) <- raster::extent(dat) + 2*dpad

  return(dat_pad)
}
