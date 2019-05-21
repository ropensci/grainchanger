#' Pad a raster by a specified radius
#'
#' This function pads a raster by a specified number of cells, creating the effect of a
#' torus. Allows for moving window analysis that avoids edge effects (e.g. on simulated
#' landscapes).
#'
#' @param dat The raster dataset to pad
#' @param r The radius by which to pad the raster
#'
#' @return raster. Original raster padded by radius r with torus effect
#'
#' @keywords torus, raster
#'
#' @examples
#' data(cat_ls)
#' d <- create_torus(dat = cat_ls, r = 5)
#' @export

create_torus <- function(dat, r) {
  # This function takes as input a raster and an integer radius value.
  checkmate::assert_class(dat, "RasterLayer")
  checkmate::assert_numeric(r)

  resolution <- raster::res(dat)[1]
  r <- r / resolution

  #   1. Convert raster to matrix
  dat_m <- raster::as.matrix(dat)
  nrows <- nrow(dat_m)
  ncols <- ncol(dat_m)

  #   2. Create new matrix of dim + radius*2
  dat_pad_m <- matrix(NA, nrow = nrows + 2 * r, ncol = ncols + 2 * r)

  #   3. Infill with values from the matrix
  # top
  dat_pad_m[1:r, (r + 1):(ncols + r)] <- 
    dat_m[(nrows - r + 1):nrows, ]
  # left
  dat_pad_m[(r + 1):(nrows + r), 1:r] <- 
    dat_m[, (ncols - r + 1):ncols]
  # bottom
  dat_pad_m[(nrows + r + 1):(nrows + 2 * r), (r + 1):(ncols + r)] <- 
    dat_m[1:r, ]
  # right
  dat_pad_m[(r + 1):(nrows + r), (ncols + r + 1):(ncols + 2 * r)] <- 
    dat_m[, 1:r]
  # top left corner
  dat_pad_m[1:r, 1:r] <- 
    dat_m[(nrows - r + 1):nrows, (ncols - r + 1):ncols]
  # top right corner
  dat_pad_m[1:r, (ncols + r + 1):(ncols + 2 * r)] <- 
    dat_m[(nrows - r + 1):nrows, 1:r]
  # bottom left corner
  dat_pad_m[(nrows + r + 1):(nrows + 2 * r), 1:r] <- 
    dat_m[1:r, (ncols - r + 1):ncols]
  # bottom right corner
  dat_pad_m[(nrows + r + 1):(nrows + 2 * r), (ncols + r + 1):(ncols + 2 * r)] <- 
    dat_m[1:r, 1:r]
  # centre
  dat_pad_m[(r + 1):(nrows + r), (r + 1):(ncols + r)] <- 
    dat_m

  #   4. convert to raster
  dat_pad <- raster::raster(dat_pad_m)

  #   5. Fix resolution
  # specify resolution ----
  raster::extent(dat_pad) <- c(
    0,
    ncol(dat_pad) * resolution,
    0,
    nrow(dat_pad) * resolution
  )

  return(dat_pad)
}
