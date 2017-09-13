# packages
library(rgdal)
library(raster)

# load source code
source("code/winmoveR_functions.R")

# load LCM
lcm <- raster("../2_Real_Landscapes/data/lcm/lcm2007_25m_gb.tif")

# load BNG
bng <- readOGR("../2_Real_Landscapes/data/bng", "10km_grid_region")

# get SK45 to experiment on 
bng_sml <- subset(bng, TILE_NAME=="SK45")
lcm_sml <- crop(lcm, bng_sml)

# create tiny raster for initial testing
m <- matrix(1:100, nrow=10, byrow = TRUE)
r <- raster(m)
plot(r)

r_dt <- as.data.table.raster(r, inmem = FALSE)

w <- 5
# using fixed i at present - this will be all the cells we are interested in
# getting a focal measure for
i <- 23

fnWinInd <- function(i, w, x) { # w = window size, i = cell, x = number of columns in raster
  # from a given w, we set the row/column indices as +/- the number of cells
  # either side of the focal cell
  w_i <- -floor(w/2):floor(w/2)
  
  # we need to know the cells from the row on which the focal cell is located - we
  # repeat this w times (size of window) so we can then apply the operation of
  # adding the number of columns to get the correct index for each row
  i_row <- rep((i + w_i), w)
  
  # This lets us get the amount we need to add on to each row - it needs to be the
  # same length as `i_row` and so each value is repeated w times
  row_add <- rep(w_i * x, each=w)
  
  # Just add i_row and row_add to get the index for each cell in the window based
  # on the 2D data.table
  window <- i_row + row_add 
  
  return(window)
}


# work out how to apply this to the data.table
mw_fun <- function(dt, cells, w, r, fn) {
  x_count <- dim(r)[2]
  cells <- c(23, 24, 35)
  test <- lapply(cells, function(cell) {
    win_ind <- fnWinInd(cell, w, x_count)
    win_vals <- get(fn)(dt[dt[[1]] %in% win_ind]) 
    # got to here: the line above doesn't work - need to get the extract from the datatable into a vector
    # will need to get the results back into data.table format, then potentially get the dt back into raster
    
    })
}

